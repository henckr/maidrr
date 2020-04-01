#' Segmentation of the data
#'
#' Segmentation of observations based on the grouping of feature effects.
#'
#' @param fx_vars A list of data frames containing the feature effects.
#' @param data Data frame containing the original training data.
#' @param lambda A numeric (same lambda for all features in \code{fx_vars}) or a
#'   numeric vector of \code{length(fx_vars)} (for feature-specific lambdas).
#' @return A tidy data frame (i.e., a "tibble" object) with the segmented data.
#'   The grouped features all have a trailing underscore in their name.
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
#' gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel'),
#'                      data = mtpl_be,
#'                      interactions = 'auto',
#'                      hcut = 0.7,
#'                      pred_fun = gbm_fun) %>%
#'             segmentation(data = mtpl_be,
#'                          lambda = 0.01)
#' }
#' @export
segmentation <- function(fx_vars, data, lambda) {

  if (! length(lambda) %in% c(1, length(fx_vars))) stop('Lambda must either be a single numeric value of a numeric vector of the same length as fx_vars.')
  if (length(lambda) == 1) lambda <- rep(lambda, length(fx_vars))

  # Group each effect and add the feature to the data in a categorical format
  for (i in seq_len(length(fx_vars))) {
    fx_var <- fx_vars[[i]]
    n_grps <- fx_var %>% optimal_ngroups(lambda = lambda[i])
    fx_grp <- fx_var %>% group_pd(ngroups = n_grps)
    var <- fx_var %>% comment

    data <- data %>% dplyr::left_join(fx_grp[c(paste0('x', if (grepl('_', var)) 1:2), 'xgrp')],
                                      by = setNames(paste0('x', if (grepl('_', var)) 1:2), unlist(strsplit(var, '_')))) %>%
      dplyr::mutate(xgrp = relevel(as.factor(xgrp), ref =  (fx_grp %>% dplyr::arrange(-wgrp) %>% dplyr::pull(xgrp))[1])) %>%
      dplyr::rename(!!paste0(var, '_') := xgrp)
  }
  return(data)
}
