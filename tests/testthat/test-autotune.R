context('Automatic tuning')
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



test_that('output is of the expected format for only main effects', {
    lmbd_tune <- gbm_fit %>% autotune(data = mtpl_be,
                                      vars = c('ageph', 'bm', 'coverage', 'fuel', 'sex', 'fleet', 'use'),
                                      target = 'nclaims',
                                      hcut = -1,
                                      pred_fun = gbm_fun,
                                      lambdas = as.vector(outer(seq(1, 10, 1), 10^(-4:-2))),
                                      nfolds = 5,
                                      strat_vars = c('nclaims', 'expo'),
                                      glm_par = alist(family = poisson(link = 'log'),
                                                      offset = log(expo)),
                                      err_fun = poi_dev,
                                      ncores = -1)

    expect_is(lmbd_tune, 'list')
    expect_is(lmbd_tune$slct_feat, 'integer')
    expect_is(lmbd_tune$best_surr, 'glm')
    expect_is(lmbd_tune$tune_main, 'tbl_df')
    expect_null(lmbd_tune$tune_intr)
    expect_equal(sum(grepl('_', names(lmbd_tune$slct_feat))), 0)
})


test_that('output is of the expected format when including interactions', {
  lmbd_tune <- gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'bm', 'coverage', 'fuel', 'sex', 'fleet', 'use'),
                                    target = 'nclaims',
                                    hcut = 0.75,
                                    pred_fun = gbm_fun,
                                    lambdas = as.vector(outer(seq(1, 10, 1), 10^(-4:-2))),
                                    nfolds = 5,
                                    strat_vars = c('nclaims', 'expo'),
                                    glm_par = alist(family = poisson(link = 'log'),
                                                    offset = log(expo)),
                                    err_fun = poi_dev,
                                    ncores = -1)

  expect_is(lmbd_tune, 'list')
  expect_is(lmbd_tune$slct_feat, 'integer')
  expect_is(lmbd_tune$best_surr, 'glm')
  expect_is(lmbd_tune$tune_main, 'tbl_df')
  expect_is(lmbd_tune$tune_intr, 'tbl_df')
})

test_that('all inputs are checked accordingly', {
  expect_error(gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'bm_power'),
                                    target = 'nclaims'),
               'No underscores allowed in the variable names, these are interpreted as interactions in maidrr.')
  expect_error(gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'license'),
                                    target = 'nclaims'),
               'All the variables needs to be present in the data.')
  expect_error(gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'coverage'),
                                    target = 'numclaims'),
               'The target variable needs to be present in the data.')
  expect_error(gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'coverage'),
                                    target = 'nclaims',
                                    hcut = 1.5),
               'Invalid value specified for hcut, must equal -1 or lie within the range \\[0, 1\\].')
  expect_error(gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'coverage'),
                                    target = 'nclaims',
                                    nfolds = 1),
               'At least two folds are needed to perform K-fold cross-validation.')
  expect_error(gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'coverage'),
                                    target = 'nclaims',
                                    strat_vars = 'wrong_var'),
               'The stratification variables in strat_vars need to be present in the data.')
  expect_error(gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'coverage'),
                                    target = 'nclaims',
                                    err_fun = function(y_pred) mean(y_pred)),
               'The error function must contain arguments y_true and y_pred.')
  expect_error(gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'coverage'),
                                    target = 'nclaims',
                                    err_fun = function(y_pred, y_true, y_other) mean(y_pred)),
               'The error function can only contain arguments y_true, y_pred and w_case.')
  expect_error(gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'coverage'),
                                    target = 'nclaims',
                                    err_fun = function(y_pred, y_true, w_case) mean(y_pred)),
               'If w_case is an argument in err_fun, weights must be an argument in glm_par.')
  expect_error(gbm_fit %>% autotune(data = mtpl_be,
                                    vars = c('ageph', 'coverage'),
                                    target = 'nclaims',
                                    ncores = 0),
               'The number of cores must be strictly positive, or equal to -1 for all available cores.')
})
