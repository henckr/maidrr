#' Autotuned surrogate
#'
#' Fit the optimal surrogate GLM based on the automated tuning results.
#'
#' @param autotune_out A tidy data frame with the results from \code{\link{autotune}}.
#' @inheritParams autotune
#' @return The optimal GLM (i.e., a "glm" object), according to \code{\link{autotune}}.
#' @examples
#' \dontrun{
#' data('mtpl_be')
#' features <- setdiff(names(mtpl_be),c('id', 'nclaims', 'expo'))
#' set.seed(12345)
#' gbm_fit <- gbm::gbm(as.formula(paste('nclaims ~',
#'                                paste(features, sep = ' ', collapse = ' + '))),
#'                     distribution = 'poisson',
#'                     data = mtpl_be,
#'                     n.trees = 50,
#'                     interaction.depth = 3,
#'                     shrinkage = 0.1)
#' gbm_fun <- function(object, newdata) mean(predict(object, newdata, n.trees = object$n.trees, type = 'response'))
#' fx_vars <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel'),
#'                                 data = mtpl_be,
#'                                 interactions = 'auto',
#'                                 hcut = 0.7,
#'                                 pred_fun = gbm_fun)
#' fx_vars %>% autotune(data = mtpl_be,
#'                      target = 'nclaims',
#'                      lambdas = lambda_grid(., lambda_range = as.vector(outer(seq(1, 10, 1), 10^(-3:-1)))),
#'                      nfolds = 5,
#'                      strat_vars = c('nclaims', 'expo'),
#'                      glm_par = alist(family = poisson(link = 'log'),
#'                                      offset = log(expo)),
#'                      err_fun = poi_dev,
#'                      ncores = -1) %>%
#'              autofit(fx_vars = fx_vars,
#'                      data = mtpl_be,
#'                      target = 'nclaims',
#'                      glm_par = alist(family = poisson(link = 'log'),
#'                                      offset = log(expo)))
#'
#' }
#' @export
autofit <- function(autotune_out, fx_vars, data, target, glm_par = alist()){

  # Arrange the autotune results from best to worst
  autotune_out <- autotune_out %>% dplyr::arrange(cv_err)

  # Segmentation for optimal lambda values
  data_segm <- fx_vars %>% segmentation(data = data, lambda = ifelse(grepl('_', lapply(., comment)),
                                                                     autotune_out[[1, 'intr']], autotune_out[[1, 'main']]))

  # Feature selection (only keep those with >1 group) and determine the GLM formula
  slct_feat <- names(which(unlist(lapply(data_segm[, grepl('_$', names(data_segm))], function(x) length(unique(x)) > 1))))
  glm_par[['formula']] <- as.formula(paste(target, '~', paste(c(1, slct_feat), collapse = ' + ')))

  # Fit the surrogate GLM to the segmented data
  data_segm %>% surrogate(glm_par)
}
