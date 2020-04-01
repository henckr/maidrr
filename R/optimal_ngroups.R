#' Optimal number of groups
#'
#' Determine the optimal number of groups for a feature.
#'
#' @param pd A data frame containing the partial dependence function as returned
#'   by \code{\link{get_pd}}.
#' @param lambda The complexity parameter in the penalized loss function.
#' @param search_grid An integer vector containing the values to evaluate.
#' @return An integer specifying the optimal number of groups. When multiple
#'   groupings lead to the lowest loss, the smallest value is returned.
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
#' gbm_fit %>% get_pd(var = 'ageph',
#'                    grid = data.frame('ageph' = 20:90),
#'                    data = mtpl_be,
#'                    subsample = 10000,
#'                    fun = gbm_fun) %>%
#'             optimal_ngroups(lambda = 0.01)
#' }
#' @export
optimal_ngroups <- function(pd, lambda, search_grid = seq_len(min(length(unique(pd$y)), 15))) {

  if (length(search_grid) == 0) stop('Search grid should contain at least one value.')
  if (! all(search_grid %>% purrr::map(function(x) x%%1 == 0) %>% unlist)) {
    warning('Non-integers are supplied in search_grid. These are converted to integers so interpret the result with care.')
    search_grid <- search_grid %>% as.integer
  }

  min_ind <- search_grid %>% purrr::map(function(i) loss_func(pd, lambda, i)) %>% which.min()
  return(search_grid[min_ind])
}


loss_func <- function(pd, lambda, ngroups){

  pd_grp <- pd %>% group_pd(ngroups = ngroups)

  if (pd_grp %>% is.null) return(Inf)

  pd_grp %>% dplyr::ungroup() %>%
    #dplyr::summarize(loss = (sum(w * abs(y - ygrp))/sum(w)) + (lambda * log10(length(unique(xgrp))))) %>%
    dplyr::summarize(loss = (mean((y - ygrp)^2)) + (lambda * log10(length(unique(xgrp))))) %>%
    dplyr::pull(loss)
}
