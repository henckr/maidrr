#' Pure two-way interaction
#'
#' Compute the pure two-way interaction partial dependence function for two
#' features from the 2D partial dependence and both 1D partial dependencies.
#'
#' @param pd_2d Data frame containing the 2D partial dependence as returned by
#'   \code{\link{get_pd}} with \code{var = 'var1_var2'}.
#' @param pd_1d List of data frames containing the 1D partial dependence for
#'   var1 and var2 as returned by \code{\link{get_pd}} with \code{var = 'var1'}
#'   and \code{var = 'var2'} respectively.
#' @return Tidy data frame (i.e., a "tibble" object) with four (x1, x2, y, w)
#'   columns. Columns \code{x} contain variable values, column \code{y} the
#'   pure interaction effect and \code{w} the observation counts.
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
#' pd_2d <- get_pd(mfit = gbm_fit,
#'                 var = 'ageph_coverage',
#'                 grid = tidyr::expand_grid('ageph' %>% get_grid(data = mtpl_be),
#'                                           'coverage' %>% get_grid(data = mtpl_be)),
#'                 data = mtpl_be,
#'                 subsample = 10000,
#'                 fun = gbm_fun)
#' pd_1d <- list(get_pd(mfit = gbm_fit,
#'                      var = 'ageph',
#'                      grid = 'ageph' %>% get_grid(data = mtpl_be),
#'                      data = mtpl_be,
#'                      subsample = 10000,
#'                      fun = gbm_fun),
#'               get_pd(mfit = gbm_fit,
#'                      var = 'coverage',
#'                      grid = 'coverage' %>% get_grid(data = mtpl_be),
#'                      data = mtpl_be,
#'                      subsample = 10000,
#'                      fun = gbm_fun))
#' interaction_pd(pd_2d, pd_1d)
#' }
#' @export
interaction_pd <- function(pd_2d, pd_1d) {

  if (! all(unlist(strsplit(comment(pd_2d), '_')) %in% unlist(lapply(pd_1d, comment)))) stop('Not all variables of pd_2d are present in pd_1d.')
  vrb <- comment(pd_2d)

  # Get the correct marginal PDs from the list
  pd_var1 <- pd_1d[[which(unlist(lapply(pd_1d, comment)) == unlist(strsplit(comment(pd_2d), '_'))[[1]])]]
  pd_var2 <- pd_1d[[which(unlist(lapply(pd_1d, comment)) == unlist(strsplit(comment(pd_2d), '_'))[[2]])]]

  if (! all(pd_2d$x1 %in% pd_var1$x)) stop(sprintf('Not all grid values for %s are present in the 1D partial dependence.', unlist(strsplit(comment(pd_2d), '_'))[[1]]))
  if (! all(pd_2d$x2 %in% pd_var2$x)) stop(sprintf('Not all grid values for %s are present in the 1D partial dependence.', unlist(strsplit(comment(pd_2d), '_'))[[2]]))

  # Join the partial dependencies and calculate the pure interaction effect
  pd_2d <- pd_2d %>% dplyr::left_join(y = pd_var1[c('x', 'y')], by = c('x1' = 'x'), suffix = c('','1')) %>%
                     dplyr::left_join(y = pd_var2[c('x', 'y')], by = c('x2' = 'x'), suffix = c('','2')) %>%
                     dplyr::mutate(y = y - mean(y), y1 = y1 - mean(y1), y2 = y2 - mean(y2)) %>%
                     dplyr::mutate(y = y - y1 - y2) %>%
                     dplyr::select(-c(y1, y2))

  comment(pd_2d) <- vrb

  return(pd_2d)
}
