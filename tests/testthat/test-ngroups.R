context('Number of groups')
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
  ngrp <- gbm_fit %>% get_pd(var = 'ageph',
                             grid = data.frame('ageph' = 20:80),
                             data = mtpl_be,
                             subsample = 1000,
                             fun = gbm_fun) %>%
    optimal_ngroups(lambda = 0.0001)

  expect_is(ngrp, 'integer')
  expect_equal(ngrp, 3)
})


test_that('an error is produced when the search grid is empty', {
  expect_error(gbm_fit %>% get_pd(var = 'ageph',
                                  grid = data.frame('ageph' = 20:80),
                                  data = mtpl_be,
                                  subsample = 1000,
                                  fun = gbm_fun) %>%
                 optimal_ngroups(lambda = 0.0001, search_grid = integer()),
               'Search grid should contain at least one value.')
})


test_that('a warning is produced when there are non-integers present in the search grid, but still with output', {
  expect_warning(gbm_fit %>% get_pd(var = 'ageph',
                                    grid = data.frame('ageph' = 20:80),
                                    data = mtpl_be,
                                    subsample = 1000,
                                    fun = gbm_fun) %>%
                   optimal_ngroups(lambda = 0.0001, search_grid = c(1, 2.3, 5.6, 8)),
                 'Non-integers are supplied in search_grid. These are converted to integers so interpret the result with care.')
  expect_equal(suppressWarnings(gbm_fit %>% get_pd(var = 'ageph',
                                                   grid = data.frame('ageph' = 20:80),
                                                   data = mtpl_be,
                                                   subsample = 1000,
                                                   fun = gbm_fun) %>%
                                  optimal_ngroups(lambda = 0.0001, search_grid = c(1, 2.3, 5.6, 8))), 2)
})
