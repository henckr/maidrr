context('Model insights')
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


test_that('output is of the expected format', {
  mdl_insights <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'),
                                       data = mtpl_be,
                                       interactions = 'user',
                                       pred_fun = gbm_fun)

  expect_is(mdl_insights, 'list')
  expect_equal(length(mdl_insights), 6)
  expect_is(mdl_insights[[1]], 'tbl_df')
  expect_true(all(unlist(lapply(mdl_insights, comment)) %in% c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage')))
  expect_is(mdl_insights[['ageph']]$x, 'integer')
  expect_is(mdl_insights[['fuel']]$x, 'factor')
  expect_is(mdl_insights[['ageph_coverage']]$x1, 'integer')
  expect_is(mdl_insights[['ageph_coverage']]$x2, 'factor')
  expect_equal(sum(unlist(lapply(mdl_insights, function(i) sum(is.na(i))))), 0)
  expect_true(all(unlist(lapply(mdl_insights, function(i) sum(i$w))) == nrow(mtpl_be)))
})


test_that('an error is produced when features are not present in the data', {
  expect_error(gbm_fit %>% insights(vars = c('ageph', 'power', 'license'),
                                    data = mtpl_be),
               'Some features specified in vars can not be found in the data.')
})


test_that('it works without interactions when interactions = "user"', {
  mdl_insights <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel'),
                                       data = mtpl_be,
                                       interactions = 'user',
                                       pred_fun = gbm_fun)

  expect_false(any(grepl('_', unlist(lapply(mdl_insights, comment)))))
  expect_is(mdl_insights, 'list')
  expect_equal(length(mdl_insights), 4)
  expect_is(mdl_insights[[1]], 'tbl_df')
  expect_true(all(unlist(lapply(mdl_insights, comment)) %in% c('ageph', 'bm', 'coverage', 'fuel')))
  expect_is(mdl_insights[['ageph']]$x, 'integer')
  expect_is(mdl_insights[['fuel']]$x, 'factor')
  expect_equal(sum(unlist(lapply(mdl_insights, function(i) sum(is.na(i))))), 0)
  expect_true(all(unlist(lapply(mdl_insights, function(i) sum(i$w))) == nrow(mtpl_be)))
})


test_that('interactions are handled properly when interactions = "user"', {
  expect_error(gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage', 'ageph_sex'),
                                    data = mtpl_be,
                                    interactions = 'user',
                                    pred_fun = gbm_fun),
               'Each feature that is included in an interaction should also be present as a main effect.')

  mdl_insights <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'),
                                       data = mtpl_be,
                                       interactions = 'user',
                                       pred_fun = gbm_fun)
  expect_equal(length(mdl_insights), 6)
  expect_equal(sum(grepl('_', unlist(lapply(mdl_insights, comment)))), 2)
})


test_that('interactions are handled properly when interactions = "auto"', {
  expect_warning(gbm_fit %>% insights(vars = c('bm', 'coverage', 'fuel', 'bm_fuel', 'bm_coverage'),
                                      data = mtpl_be,
                                      interactions = 'auto',
                                      pred_fun = gbm_fun),
                 'Interactions specified in vars are ignored when interactions = "auto".')

  expect_error(gbm_fit %>% insights(vars = c('bm', 'coverage', 'fuel'),
                                    data = mtpl_be,
                                    interactions = 'auto',
                                    hcut = 2,
                                    pred_fun = gbm_fun),
               'The parameter hcut must lie within the range \\[0, 1\\].')

  mdl_insights <- gbm_fit %>% insights(vars = c('bm', 'coverage', 'fuel'),
                                       data = mtpl_be,
                                       interactions = 'auto',
                                       hcut = 0,
                                       pred_fun = gbm_fun)

  expect_equal(length(mdl_insights), 4)
  expect_equal(sum(grepl('_', unlist(lapply(mdl_insights, comment)))), 1)

  mdl_insights <- gbm_fit %>% insights(vars = c('bm', 'coverage', 'fuel'),
                                       data = mtpl_be,
                                       interactions = 'auto',
                                       hcut = 1,
                                       pred_fun = gbm_fun)

  expect_equal(length(mdl_insights), 6)
  expect_equal(sum(grepl('_', unlist(lapply(mdl_insights, comment)))), 3)
})


test_that('effects supplied in fx_in are handled properly when interactions = "user"', {

  fx_pre <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'),
                                 data = mtpl_be,
                                 interactions = 'user',
                                 pred_fun = gbm_fun)

  mdl_insights <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'),
                                       data = mtpl_be,
                                       interactions = 'user',
                                       pred_fun = gbm_fun,
                                       fx_in = fx_pre)

  expect_true(all(unlist(lapply(c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'), function(v) all(fx_pre[[v]] == mdl_insights[[v]])))))
  expect_is(mdl_insights, 'list')
  expect_equal(length(mdl_insights), 6)
  expect_is(mdl_insights[[1]], 'tbl_df')
  expect_true(all(unlist(lapply(mdl_insights, comment)) %in% c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage')))
  expect_equal(sum(unlist(lapply(mdl_insights, function(i) sum(is.na(i))))), 0)
  expect_true(all(unlist(lapply(mdl_insights, function(i) sum(i$w))) == nrow(mtpl_be)))

  mdl_insights <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'),
                                       data = mtpl_be,
                                       interactions = 'user',
                                       pred_fun = gbm_fun,
                                       fx_in = fx_pre[c('ageph', 'bm', 'coverage')])

  expect_true(all(unlist(lapply(c('ageph', 'bm', 'coverage'), function(v) all(fx_pre[[v]] == mdl_insights[[v]])))))
  expect_is(mdl_insights, 'list')
  expect_equal(length(mdl_insights), 6)
  expect_is(mdl_insights[[1]], 'tbl_df')
  expect_true(all(unlist(lapply(mdl_insights, comment)) %in% c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage')))
  expect_equal(sum(unlist(lapply(mdl_insights, function(i) sum(is.na(i))))), 0)
  expect_true(all(unlist(lapply(mdl_insights, function(i) sum(i$w))) == nrow(mtpl_be)))
})


test_that('effects supplied in fx_in are handled properly when interactions = "auto"', {
  fx_pre <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'),
                                 data = mtpl_be,
                                 interactions = 'user',
                                 pred_fun = gbm_fun)

  expect_warning(gbm_fit %>% insights(vars = c('bm', 'coverage', 'fuel'),
                                      data = mtpl_be,
                                      interactions = 'auto',
                                      pred_fun = gbm_fun,
                                      fx_in = fx_pre),
                 'Interactions specified in fx_in are ignored when interactions = "auto".')

  mdl_insights <- gbm_fit %>% insights(vars = c('bm', 'coverage', 'fuel'),
                                       data = mtpl_be,
                                       interactions = 'auto',
                                       pred_fun = gbm_fun,
                                       fx_in = fx_pre[c('bm', 'coverage', 'fuel')])

  expect_is(mdl_insights, 'list')
  expect_equal(length(mdl_insights), 5)
  expect_is(mdl_insights[[1]], 'tbl_df')
  expect_true(all(unlist(lapply(mdl_insights, comment)) %in% c('bm', 'coverage', 'fuel', 'bm_fuel', 'coverage_fuel')))
  expect_equal(sum(unlist(lapply(mdl_insights, function(i) sum(is.na(i))))), 0)
  expect_true(all(unlist(lapply(mdl_insights, function(i) sum(i$w))) == nrow(mtpl_be)))
})
