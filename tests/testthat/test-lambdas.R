context('Lambda grid')
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


test_that('output is of the expected format (only main effects)', {
  lmbd_grid <- fx_vars[! grepl('_', names(fx_vars))] %>% lambda_grid(lambda_range = as.vector(outer(seq(1, 10, 1), 10^(-3:-1))))

  expect_is(lmbd_grid, 'tbl_df')
  expect_equal(nrow(lmbd_grid), length(unique(lmbd_grid$lambda_main)))
  expect_equal(ncol(lmbd_grid), 1 + sum(! grepl('_', names(fx_vars))))
  expect_true(all(names(lmbd_grid) %in% c('lambda_main', names(fx_vars)[!grepl('_', names(fx_vars))])))
  expect_equal(sum(is.na(lmbd_grid)), 0)
})


test_that('output is of the expected format (only interaction effects)', {
  lmbd_grid <- fx_vars[grepl('_', names(fx_vars))] %>% lambda_grid(lambda_range = as.vector(outer(seq(1, 10, 1), 10^(-3:-1))))

  expect_is(lmbd_grid, 'tbl_df')
  expect_equal(nrow(lmbd_grid), length(unique(lmbd_grid$lambda_intr)))
  expect_equal(ncol(lmbd_grid), 1 + sum(grepl('_', names(fx_vars))))
  expect_true(all(names(lmbd_grid) %in% c('lambda_intr', names(fx_vars)[grepl('_', names(fx_vars))])))
  expect_equal(sum(is.na(lmbd_grid)), 0)
})


test_that('output is of the expected format (both main and interaction effects)', {
  lmbd_grid <- fx_vars %>% lambda_grid(lambda_range = as.vector(outer(seq(1, 10, 1), 10^(-3:-1))))

  expect_is(lmbd_grid, 'tbl_df')
  expect_equal(nrow(lmbd_grid), length(unique(lmbd_grid$lambda_main)) * length(unique(lmbd_grid$lambda_intr)))
  expect_equal(ncol(lmbd_grid), 2 + length(names(fx_vars)))
  expect_true(all(names(lmbd_grid) %in% c('lambda_main', 'lambda_intr', names(fx_vars))))
  expect_equal(sum(is.na(lmbd_grid)), 0)
})


