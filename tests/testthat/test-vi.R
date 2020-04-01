context('Variable importance')
library(maidrr)

# Use a gbm fit on the mtpl_be data to test the variable importance function
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


test_that('output is of the expected format', {
  var_imp <- gbm_fit %>% get_vi
  expect_is(var_imp, 'tbl_df')
  expect_equal(ncol(var_imp), 2)
  expect_equal(nrow(var_imp), length(features))
  expect_true(all(c('Variable', 'Importance') %in% names(var_imp)))
  expect_true(all(features %in% var_imp$Variable))
})

test_that('additional arguments are passed on', {
  expect_true(all(diff(get_vi(gbm_fit, decreasing = FALSE)$Importance) >= 0))
  expect_equal(max(get_vi(gbm_fit, scale = TRUE)$Importance), 100)
})
