#' Partial dependence grouping
#'
#' Grouping of feature values/levels by binning continuous/ordinal features and
#' clustering nominal features. Partial dependencies are used to perform the
#' grouping in a data-driven way.
#'
#' @param pd A data frame containing the partial dependence function as returned
#'   by \code{\link{get_pd}}.
#' @param ngroups An integer specifying the number of groups.
#' @return The tidy data frame (i.e., a "tibble" object) supplied in \code{pd}
#'   with three additional columns: xgrp, ygrp and wgrp. Column \code{xgrp}
#'   contains feature groups, column \code{ygrp} the average partial dependence
#'   for the group and \code{wgrp} the sum of observation counts for the group.
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
#'             group_pd(ngroups = 5)
#' }
#' @export
group_pd <- function(pd, ngroups) {

  # One-dimensional partial dependence
  if ('x' %in% names(pd)) {
    # Continuous or ordinal feature
    if (any(c('integer', 'numeric', 'ordered') %in% class(pd$x))) return(group_pd_ckseg(pd, ngroups))
    # Nominal feature
    if ('factor' %in% class(pd$x)) return(group_pd_ckmns(pd, ngroups))

    stop('Unsupported variable type. Only integers, numerics and factors are handled by this function.')
  }

  # Two-dimensional partial dependence
  if (all(c('x1', 'x2') %in% names(pd))) return(group_pd_ckmns(pd, ngroups))

  stop('The pd data frame is supplied in a wrong format (either col x or cols x1 and x2 needed, see doc of get_pd).')
}


#' @describeIn group_pd Grouping via \code{\link[Ckmeans.1d.dp]{Cksegs.1d.dp}}.
#' @export
group_pd_ckseg <- function(pd, ngroups) {

  if (ngroups > nrow(pd)) {
    warning(sprintf('It was not possible to group %s in %i groups, returned NULL.', comment(pd), ngroups))
    return(NULL)
  }

  vrb <- comment(pd)

  # Perform the clustering
  clust <- Ckmeans.1d.dp::Cksegs.1d.dp(y = pd$y, k = ngroups, x = pd$x)

  # Added grouped values to the partial dependence
  pd <- pd %>% dplyr::mutate(clust = clust$cluster) %>%
    dplyr::group_by(clust) %>%
    dplyr::mutate(ygrp = mean(y)) %>%
    dplyr::mutate(wgrp = sum(w)) %>%
    dplyr::mutate(xgrp = paste('[', round(min(as.numeric(x)), digits = 2), ', ', round(max(as.numeric(x)), digits = 2), ']', sep = ''))

  comment(pd) <- vrb

  return(pd)
}


#' @describeIn group_pd Grouping via \code{\link[Ckmeans.1d.dp]{Ckmeans.1d.dp}}.
#' @export
group_pd_ckmns <- function(pd, ngroups) {

  if (ngroups > nrow(pd)) {
    warning(sprintf('It was not possible to group %s in %i groups, returned NULL.', comment(pd), ngroups))
    return(NULL)
  }

  vrb <- comment(pd)

  # Perform the clustering
  clust <- Ckmeans.1d.dp::Ckmeans.1d.dp(x = pd$y, k = ngroups)

  # Added grouped values to the partial dependence
  pd <- pd %>% dplyr::mutate(clust = clust$cluster) %>%
    dplyr::group_by(clust) %>%
    dplyr::mutate(ygrp = mean(y)) %>%
    dplyr::mutate(wgrp = sum(w))
  if ('x' %in% names(pd)) pd <- pd %>% dplyr::mutate(xgrp = paste0('{', paste(x, collapse = ', '), '}'))
  if (all(c('x1', 'x2') %in% names(pd))) pd <- pd %>% dplyr::mutate(xgrp = dplyr::group_indices())

  comment(pd) <- vrb

  return(pd)
}




# group_pd_rpart <- function(pd, ngroups) {
#
#   # Fit a deep regression tree
#   deep_tree <- rpart::rpart(y ~ x, data = pd, weights = w, method = 'anova',
#                             control = rpart::rpart.control(minsplit = 2, maxdepth = 10, xval = 0, cp = 0))
#
#   # Check if the asked number of segments is possible
#   if (! is.element(ngroups - 1, deep_tree$cptable[, 'nsplit'])) {
#     warning(sprintf('It was not possible to group %s in %i groups, returned NULL.', comment(pd), ngroups))
#     return(NULL)
#   }
#
#   vrb <- comment(pd)
#
#   # Get the cp value and prune the tree
#   cp_val <- deep_tree$cptable %>% tibble::as_tibble() %>% dplyr::filter(nsplit == ngroups - 1) %>% dplyr::pull(CP)
#   tree <- deep_tree %>% rpart::prune(cp = cp_val)
#
#   # Add grouped values to the partial dependence
#   pd <- pd %>% dplyr::mutate(ygrp = predict(tree)) %>%
#     dplyr::group_by(ygrp) %>%
#     dplyr::mutate(wgrp = sum(w)) %>%
#     dplyr::mutate(xgrp = paste('[', round(min(as.numeric(x)), digits = 2), ', ', round(max(as.numeric(x)), digits = 2), ']', sep = ''))
#
#   comment(pd) <- vrb
#
#   return(pd)
# }
