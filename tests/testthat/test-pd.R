context('Partial dependence')
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


test_that('output is of the expected format (marginal)', {
  par_dep <- gbm_fit %>% get_pd(var = 'ageph',
                                grid = data.frame('ageph' = 30:40),
                                data = mtpl_be,
                                subsample = 1000,
                                fun = gbm_fun)
  expect_is(par_dep, 'tbl_df')
  expect_equal(ncol(par_dep), 3)
  expect_equal(nrow(par_dep), 11)
  expect_true(all(c('x', 'y', 'w') %in% names(par_dep)))
  expect_is(par_dep$x, 'integer')
  expect_equal(sum(is.na(par_dep)), 0)
  expect_match(comment(par_dep), 'ageph')
})


test_that('output is of the expected format (two-way interaction)', {
  par_dep <- gbm_fit %>% get_pd(var = 'power_coverage',
                                grid = expand.grid('power' = 50:55, 'coverage' = c('TPL', 'TPL+', 'TPL++')),
                                data = mtpl_be,
                                subsample = 1000,
                                fun = gbm_fun)
  expect_is(par_dep, 'tbl_df')
  expect_equal(ncol(par_dep), 4)
  expect_equal(nrow(par_dep), 18)
  expect_true(all(c('x1', 'x2', 'y', 'w') %in% names(par_dep)))
  expect_is(par_dep$x1, 'integer')
  expect_is(par_dep$x2, 'factor')
  expect_equal(sum(is.na(par_dep)), 0)
  expect_match(comment(par_dep), 'power_coverage')
})


test_that('an error is produced when interactions are specified wrongly', {
  expect_error(gbm_fit %>% get_pd(var = 'ageph_power_bm',
                                  grid = NULL,
                                  data = NULL),
               'Only two-way interactions are supported for now.')
  expect_error(gbm_fit %>% get_pd(var = c('ageph','power'),
                                  grid = NULL,
                                  data = NULL),
               'Specify interactions via the underscore, not via a character vector.')
})
