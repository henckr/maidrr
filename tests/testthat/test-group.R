context('Feature grouping')
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


test_that('output is of the expected format (marginal and continuous)', {
  pd_grp <- gbm_fit %>% get_pd(var = 'ageph',
                               grid = data.frame('ageph' = 20:70),
                               data = mtpl_be,
                               subsample = 1000,
                               fun = gbm_fun) %>%
            group_pd(ngroups = 4)
  expect_is(pd_grp, 'tbl_df')
  expect_equal(ncol(pd_grp), 7)
  expect_equal(nrow(pd_grp), 51)
  expect_true(all(c('x', 'y', 'w', 'xgrp', 'ygrp', 'wgrp') %in% names(pd_grp)))
  expect_is(pd_grp$x, 'integer')
  expect_equal(sum(is.na(pd_grp)), 0)
  expect_match(comment(pd_grp), 'ageph')
  expect_equal(length(unique(pd_grp$xgrp)), 4)
  expect_true(dplyr::is.grouped_df(pd_grp))
})


test_that('output is of the expected format (marginal and factor)', {
  pd_grp <- gbm_fit %>% get_pd(var = 'coverage',
                               grid = data.frame('coverage' = c('TPL', ' TPL+', 'TPL++')),
                               data = mtpl_be,
                               subsample = 1000,
                               fun = gbm_fun) %>%
            group_pd(ngroups = 2)
  expect_is(pd_grp, 'tbl_df')
  expect_equal(ncol(pd_grp), 7)
  expect_equal(nrow(pd_grp), 3)
  expect_true(all(c('x', 'y', 'w', 'xgrp', 'ygrp', 'wgrp') %in% names(pd_grp)))
  expect_is(pd_grp$x, 'factor')
  expect_equal(sum(is.na(pd_grp)), 0)
  expect_match(comment(pd_grp), 'coverage')
  expect_equal(length(unique(pd_grp$xgrp)), 2)
  expect_true(dplyr::is.grouped_df(pd_grp))
})


test_that('output is of the expected format (two-way interaction)', {
  pd_grp <- gbm_fit %>% get_pd(var = 'ageph_power',
                               grid = expand.grid('ageph' = 35:55, 'power' = 40:60),
                               data = mtpl_be,
                               subsample = 1000,
                               fun = gbm_fun) %>%
            group_pd(ngroups = 6)
  expect_is(pd_grp, 'tbl_df')
  expect_equal(ncol(pd_grp), 8)
  expect_equal(nrow(pd_grp), 441)
  expect_true(all(c('x1', 'x2', 'y', 'w', 'xgrp', 'ygrp', 'wgrp') %in% names(pd_grp)))
  expect_is(pd_grp$x1, 'integer')
  expect_is(pd_grp$x2, 'integer')
  expect_equal(sum(is.na(pd_grp)), 0)
  expect_match(comment(pd_grp), 'ageph_power')
  expect_true(dplyr::is.grouped_df(pd_grp))
})


test_that('an error is produced when input is of wrong format', {
  expect_error(data.frame('x' = c(TRUE, FALSE, FALSE, TRUE)) %>%
               group_pd(ngroups = 4),
               'Unsupported variable type. Only integers, numerics and factors are handled by this function.')
  expect_error(data.frame('x1' = c(TRUE, FALSE, FALSE, TRUE)) %>%
               group_pd(ngroups = 4),
               'The pd data frame is supplied in a wrong format \\(either col x or cols x1 and x2 needed, see doc of get_pd\\).')
})


test_that('a warning is produced and NULL is returned when grouping fails', {

  expect_warning(gbm_fit %>% get_pd(var = 'coverage',
                                    grid = data.frame('coverage' = c('TPL', ' TPL+', 'TPL++')),
                                    data = mtpl_be,
                                    subsample = 1000,
                                    fun = gbm_fun) %>% group_pd(ngroups = 5),
                 'It was not possible to group coverage in 5 groups, returned NULL.')

  expect_null(suppressWarnings(gbm_fit %>% get_pd(var = 'coverage',
                                                  grid = data.frame('coverage' = c('TPL', ' TPL+', 'TPL++')),
                                                  data = mtpl_be,
                                                  subsample = 1000,
                                                  fun = gbm_fun) %>% group_pd(ngroups = 5)))

  expect_warning(gbm_fit %>% get_pd(var = 'ageph',
                                    grid = data.frame('ageph' = 20:70),
                                    data = mtpl_be,
                                    subsample = 1000,
                                    fun = gbm_fun) %>% group_pd(ngroups = 100),
                 'It was not possible to group ageph in 100 groups, returned NULL.')

  expect_null(suppressWarnings(gbm_fit %>% get_pd(var = 'ageph',
                                                  grid = data.frame('ageph' = 20:70),
                                                  data = mtpl_be,
                                                  subsample = 1000,
                                                  fun = gbm_fun) %>% group_pd(ngroups = 100)))

})
