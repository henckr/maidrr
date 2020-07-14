context('Data segmentation')
library(maidrr)

# Use a gbm fit on the mtpl_be data to test the partial dependence function
if (!requireNamespace('gbm', quietly = TRUE)) {
  stop('Package "gbm" needed for this function to work. Please install it.',
       call. = FALSE)
}
data('mtpl_be')
features <- setdiff(names(mtpl_be),c('id', 'nclaims', 'expo', 'postcode'))
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

test_that('output is of the expected format when using lambdas', {
  data_segm <- fx_vars %>% segmentation(data = mtpl_be,
                                        type = 'lambdas',
                                        values = 0.0001)

  expect_is(data_segm, 'data.frame')
  expect_equal(nrow(data_segm), nrow(mtpl_be))
  expect_equal(ncol(data_segm), ncol(mtpl_be) + length(fx_vars))
  expect_true(all(paste0(c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'), '_') %in% names(data_segm)))
  expect_true(all(sapply(paste0(c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'), '_'), function(x) class(data_segm[[x]])) == 'factor'))
  expect_equal(sum(is.na(data_segm)), 0)
})

test_that('output is of the expected format when using ngroups', {
  data_segm <- fx_vars %>% segmentation(data = mtpl_be,
                                        type = 'ngroups',
                                        values = setNames(c(7, 6, 2, 2, 3, 1), c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage')))

  expect_is(data_segm, 'data.frame')
  expect_equal(nrow(data_segm), nrow(mtpl_be))
  expect_equal(ncol(data_segm), ncol(mtpl_be) + length(fx_vars))
  expect_true(all(paste0(c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'), '_') %in% names(data_segm)))
  expect_true(all(sapply(paste0(c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'), '_'), function(x) class(data_segm[[x]])) == 'factor'))
  expect_true(all(sapply(paste0(c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'), '_'), function(x) length(unique(data_segm[[x]]))) == c(7, 6, 2, 2, 3, 1)))
  expect_equal(sum(is.na(data_segm)), 0)
})


test_that('an error is produced when the wrong type of segmentation is asked', {
  expect_error(fx_vars %>% segmentation(data = mtpl_be,
                                        type = 'something_else',
                                        values = NULL),
               'The type of segmentation must be ngroups or lambdas.')
})


test_that('an error is produced when lambda is specified in the wrong format', {
  expect_error(fx_vars %>% segmentation(data = mtpl_be,
                                        type = 'lambdas',
                                        values = c(0.001, 0.00005, 0.1)),
               'Values must either be a single numeric value of a vector of the same length as fx_vars.')
  expect_error(fx_vars %>% segmentation(data = mtpl_be,
                                        type = 'ngroups',
                                        values = setNames(c(7, 6, 2, 2, 3, 1), c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_power'))),
               'The names in values must match the comment attributes of the effects in fx_vars.')
})
