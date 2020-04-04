#' Data segmentation
#'
#' Segmentation of observations based on the grouping of feature effects.
#'
#' @param fx_vars List of data frames containing the feature effects.
#' @param data Data frame containing the original training data.
#' @param type String specifying the type of segmentation. Options are:
#'   \describe{ \item{'ngroups'}{the number of groups to use for grouping the
#'   features.} \item{'lambdas'}{optimal number of groups determined by
#'   penalized loss.} }
#' @param values The values for \code{ngroups} or \code{lambdas}. This can be a
#'   numeric value (same is used for all features in \code{fx_vars}) or a named
#'   numeric vector of \code{length(fx_vars)} (for feature-specific values). In
#'   this case, the names must match the comment attributes in \code{fx_vars}.
#' @return Data frame with the segmented data. The grouped features are added to
#'   the original \code{data} and have a trailing underscore in their name.
#' @examples
#' \dontrun{
#' data('mtpl_be')
#' features <- setdiff(names(mtpl_be), c('id', 'nclaims', 'expo', 'long', 'lat'))
#' set.seed(12345)
#' gbm_fit <- gbm::gbm(as.formula(paste('nclaims ~',
#'                                paste(features, collapse = ' + '))),
#'                     distribution = 'poisson',
#'                     data = mtpl_be,
#'                     n.trees = 50,
#'                     interaction.depth = 3,
#'                     shrinkage = 0.1)
#' gbm_fun <- function(object, newdata) mean(predict(object, newdata, n.trees = object$n.trees, type = 'response'))
#' gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel'),
#'                      data = mtpl_be,
#'                      interactions = 'user',
#'                      pred_fun = gbm_fun) %>%
#'             segmentation(data = mtpl_be,
#'                          type = 'ngroups',
#'                          values = setNames(c(7, 8, 2, 2, 3), c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel')))
#' }
#' @export
segmentation <- function(fx_vars, data, type, values) {

  if (! type %in% c('ngroups', 'lambdas')) stop('The type of segmentation must be ngroups or lambdas.')

  if (! length(values) %in% c(1, length(fx_vars))) stop('Values must either be a single numeric value of a vector of the same length as fx_vars.')
  if (length(values) == 1) values <- setNames(rep(values, length(fx_vars)), unlist(lapply(fx_vars, comment)))
  if (! length(intersect(unlist(lapply(fx_vars, comment)), names(values))) == length(fx_vars)) stop('The names in values must match the comment attributes of the effects in fx_vars.')


  # Group each effect and add the feature to the data in a categorical format
  for (i in seq_len(length(fx_vars))) {
    fx_var <- fx_vars[[i]]
    var <- fx_var %>% comment
    n_grps <- switch(type,
                     'ngroups' = values[var],
                     'lambdas' = fx_var %>% optimal_ngroups(lambda = values[var]))
    fx_grp <- fx_var %>% group_pd(ngroups = n_grps)

    data <- data %>% dplyr::left_join(fx_grp[c(paste0('x', if (grepl('_', var)) 1:2), 'xgrp')],
                                      by = setNames(paste0('x', if (grepl('_', var)) 1:2), unlist(strsplit(var, '_')))) %>%
      dplyr::mutate(xgrp = relevel(as.factor(xgrp), ref =  as.character((fx_grp %>% dplyr::arrange(-wgrp) %>% dplyr::pull(xgrp))[1]))) %>%
      dplyr::rename(!!paste0(var, '_') := xgrp)
  }
  return(data)
}

