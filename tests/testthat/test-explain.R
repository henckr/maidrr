context('Surrogate explanations')
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

seg_dat <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'ageph_bm'),
                                data = mtpl_be,
                                interactions = 'user',
                                pred_fun = gbm_fun) %>%
                       segmentation(data = mtpl_be,
                                    type = 'ngroups',
                                    values = setNames(c(6, 8, 2, 2, 2), c('ageph', 'bm', 'coverage', 'fuel', 'ageph_bm')))

sur_glm <- seg_dat %>% surrogate(par_list = alist(formula = nclaims ~ ageph_ + bm_ + coverage_ + fuel_ + ageph_bm_,
                                                  family =  poisson(link = 'log'),
                                                  offset = log(expo)))


test_that('output is of the correct format', {
  sur_expl <- sur_glm %>% explain(instance = seg_dat[34, ], plt = FALSE)

  expect_is(sur_expl, 'tbl_df')
  expect_equal(ncol(sur_expl), 7)
  expect_equal(nrow(sur_expl), length(attr(sur_glm$terms, 'term.labels')))
  expect_true(all(sur_expl$term == gsub('_$', '', attr(sur_glm$terms, 'term.labels'))))
  expect_equal(sum(is.na(sur_expl)), 0)
})


test_that('a ggplot is produced when asked for it', {
  expect_is(sur_glm %>% explain(instance = seg_dat[34, ], plt = TRUE), 'ggplot')
})


test_that('an error is produced when multiple instances are provided as input', {
  expect_error(sur_glm %>% explain(instance = seg_dat[1:3, ]),
               'Can only explain one instance prediction at a time.')
})
