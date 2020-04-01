context('Pure interaction')
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


test_that('output is of the expected format', {
  pd_2d <- get_pd(mfit = gbm_fit,
                  var = 'ageph_power',
                  grid = expand.grid('ageph' = 30:40, 'power' = 50:60),
                  data = mtpl_be,
                  subsample = 1000,
                  fun = gbm_fun)
  pd_1d <- list(get_pd(mfit = gbm_fit,
                       var = 'ageph',
                       grid = data.frame('ageph' = 30:40),
                       data = mtpl_be,
                       subsample = 1000,
                       fun = gbm_fun),
                get_pd(mfit = gbm_fit,
                       var = 'power',
                       grid = data.frame('power' = 50:60),
                       data = mtpl_be,
                       subsample = 1000,
                       fun = gbm_fun))
  pd_intr <- interaction_pd(pd_2d, pd_1d)

  expect_is(pd_intr, 'tbl_df')
  expect_equal(ncol(pd_intr), 4)
  expect_equal(nrow(pd_intr), 121)
  expect_true(all(c('x1', 'x2', 'y', 'w') %in% names(pd_intr)))
  expect_is(pd_intr$x1, 'integer')
  expect_is(pd_intr$x2, 'integer')
  expect_equal(sum(is.na(pd_intr)), 0)
  expect_match(comment(pd_intr), 'ageph_power')
})



test_that('an error is produced when some marginal effect is missing', {
  pd_2d <- data.frame() ; comment(pd_2d) <- 'power_bm'
  pd_1d <- list(data.frame()) ; comment(pd_1d[[1]]) <- 'power'
  expect_error(interaction_pd(pd_2d, pd_1d),
               'Not all variables of pd_2d are present in pd_1d.')
})


test_that('an error is produced when some grid values are missing in the 1D partial dependence', {
  pd_2d <- get_pd(mfit = gbm_fit,
                  var = 'ageph_power',
                  grid = expand.grid('ageph' = 30:40, 'power' = 50:60),
                  data = mtpl_be,
                  subsample = 1000,
                  fun = gbm_fun)
  pd_1d <- list(get_pd(mfit = gbm_fit,
                       var = 'ageph',
                       grid = data.frame('ageph' = 35:40),
                       data = mtpl_be,
                       subsample = 1000,
                       fun = gbm_fun),
                get_pd(mfit = gbm_fit,
                       var = 'power',
                       grid = data.frame('power' = 50:60),
                       data = mtpl_be,
                       subsample = 1000,
                       fun = gbm_fun))

  expect_error(interaction_pd(pd_2d, pd_1d),
               'Not all grid values for ageph are present in the 1D partial dependence.')
})
