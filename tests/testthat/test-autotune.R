context('Automatic tuning')
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
    lmbd_tune <- fx_vars %>% autotune(data = mtpl_be,
                                      target = 'nclaims',
                                      lambdas = as.vector(outer(seq(1, 10, 1), 10^(-4:-2))),
                                      nfolds = 5,
                                      strat_vars = c('nclaims', 'expo'),
                                      glm_par = alist(family = poisson(link = 'log'),
                                                      offset = log(expo)),
                                      err_fun = poi_dev,
                                      ncores = -1)

    expect_is(lmbd_tune, 'tbl_df')
    expect_equal(nrow(lmbd_tune), 7)
    expect_equal(ncol(lmbd_tune), 8 + length(names(fx_vars)))
    expect_true(all(c('lambda_main', 'lambda_intr', 'cv_err', as.character(1:5), names(fx_vars)) %in% names(lmbd_tune)))
    expect_equal(sum(is.na(lmbd_tune)), 0)
})

test_that('all inputs are checked accordingly', {
  expect_error(fx_vars %>% autotune(data = mtpl_be,
                                    target = 'numclaims',
                                    lambdas = NULL),
               'The target variable needs to be present in the data.')
  expect_error(fx_vars %>% autotune(data = mtpl_be,
                                    target = 'nclaims',
                                    lambdas = NULL,
                                    nfolds = 1),
               'At least two folds are needed to perform K-fold cross-validation.')
  expect_error(fx_vars %>% autotune(data = mtpl_be,
                                    target = 'nclaims',
                                    lambdas = NULL,
                                    strat_vars = 'wrong_var'),
               'The stratification variables in strat_vars need to be present in the data.')
  expect_error(fx_vars %>% autotune(data = mtpl_be,
                                    target = 'nclaims',
                                    lambdas = NULL,
                                    err_fun = function(y_pred) mean(y_pred)),
               'The error function must contain arguments y_true and y_pred.')
  expect_error(fx_vars %>% autotune(data = mtpl_be,
                                    target = 'nclaims',
                                    lambdas = NULL,
                                    err_fun = function(y_pred, y_true, y_other) mean(y_pred)),
               'The error function can only contain arguments y_true, y_pred and w_case.')
  expect_error(fx_vars %>% autotune(data = mtpl_be,
                                    target = 'nclaims',
                                    lambdas = NULL,
                                    err_fun = function(y_pred, y_true, w_case) mean(y_pred)),
               'If w_case is an argument in err_fun, weights must be an argument in glm_par.')
  expect_error(fx_vars %>% autotune(data = mtpl_be,
                                    target = 'nclaims',
                                    lambdas = NULL,
                                    ncores = 0),
               'The number of cores must be strictly positive, or equal to -1 for all available cores.')
  expect_warning(fx_vars %>% autotune(data = mtpl_be,
                                      target = 'nclaims',
                                      lambdas = as.vector(outer(seq(1, 10, 1), 10^(-4:-2))),
                                      nfolds = 2,
                                      ncores = 10),
                 'The asked number of cores is larger than parallel::detectCores, so there might be trouble ahead.')
  expect_warning(fx_vars %>% autotune(data = mtpl_be,
                                      target = 'nclaims',
                                      lambdas = as.vector(outer(seq(1, 10, 1), 10^(-4:-2))),
                                      nfolds = 2,
                                      glm_par = alist(formula = nclaims ~ 1)),
                 'The field "formula" is removed from glm_par as this is determined by the supplied target and automatic feature selection.')
})
