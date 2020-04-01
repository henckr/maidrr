context('Data segmentation')
library(maidrr)

# Use a gbm fit on the mtpl_be data to test the partial dependence function
if (!requireNamespace('gbm', quietly = TRUE)) {
  stop('Package "gbm" needed for this function to work. Please install it.',
       call. = FALSE)
}
data('mtpl_be')
features <- setdiff(names(mtpl_be),c('id', 'nclaims', 'expo'))
set.seed(12345)
gbm_fit <- gbm::gbm(as.formula(paste('nclaims ~',
                                     paste(features, sep = ' ', collapse = ' + '))),
                    distribution = 'poisson',
                    data = mtpl_be,
                    n.trees = 50,
                    interaction.depth = 3,
                    shrinkage = 0.1)
gbm_fun <- function(object, newdata) mean(predict(object, newdata, n.trees = object$n.trees, type = 'response'))

fx_vars <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'),
                                data = mtpl_be,
                                interactions = 'user',
                                pred_fun = gbm_fun)

test_that('output is of the expected format', {
  data_segm <- fx_vars %>% segmentation(data = mtpl_be,
                                        lambda = 0.01)

  expect_is(data_segm, 'data.frame')
  expect_equal(nrow(data_segm), nrow(mtpl_be))
  expect_equal(ncol(data_segm), ncol(mtpl_be) + length(fx_vars))
  expect_true(all(paste0(c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'), '_') %in% names(data_segm)))
  expect_true(all(sapply(paste0(c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'), '_'), function(x) class(data_segm[[x]])) == 'factor'))
  expect_equal(sum(is.na(data_segm)), 0)
})


test_that('an error is produced when lambda is specified in the wrong format', {
  expect_error(fx_vars %>% segmentation(data = mtpl_be,
                                        lambda = c(0.01, 0.005, 0001)),
               'Lambda must either be a single numeric value of a numeric vector of the same length as fx_vars.')
})
