context('Surrogate GLM')
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

seg_dat <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage'),
                                data = mtpl_be,
                                interactions = 'user',
                                pred_fun = gbm_fun) %>%
  segmentation(data = mtpl_be,
               type = 'ngroups',
               values = setNames(c(7, 9, 2, 2, 2, 2), c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel', 'ageph_coverage')))

test_that('surrogate is fit properly when data is piped in', {
  sur_glm <- seg_dat %>% surrogate(par_list = alist(formula = nclaims ~ ageph_ + bm_ + coverage_ + fuel_ + bm_fuel_ + ageph_coverage_,
                                                    family =  poisson(link = 'log'),
                                                    offset = log(expo)))

  expect_is(sur_glm, 'glm')
  expect_match(sur_glm$family$family, 'poisson')
  expect_match(sur_glm$family$link, 'log')
  expect_true(all(sur_glm$offset == log(mtpl_be$expo)))
  expect_true(all(sur_glm$data == seg_dat))
})


test_that('surrogate is fit properly when data is inserted at a random location in the function call', {
  sur_glm <- surrogate(par_list = alist(formula = nclaims ~ ageph_ + bm_ + coverage_ + fuel_ + bm_fuel_ + ageph_coverage_,
                                        family =  poisson(link = 'log'),
                                        offset = log(expo)),
                       data = seg_dat)

  expect_is(sur_glm, 'glm')
  expect_match(sur_glm$family$family, 'poisson')
  expect_match(sur_glm$family$link, 'log')
  expect_true(all(sur_glm$offset == log(mtpl_be$expo)))
  expect_true(all(sur_glm$data == seg_dat))
})
