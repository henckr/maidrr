#' Get the variable importance
#'
#' Compute variable importance scores for the predictors in a model. Note that
#' \code{get_vi} is a simple wrapper for \code{\link[vip:vi]{vip::vi}}.
#'
#' @param mfit A fitted model object (e.g., a "gbm" or "randomForest" object).
#' @param ... Additional optional arguments to be passed onto
#'   \code{\link[vip:vi]{vip::vi}}.
#' @return A tidy data frame (i.e., a "tibble" object) with two columns:
#'   \code{Variable} and \code{Importance}.
#' @seealso \code{\link[vip]{vi}} and
#'   \url{https://CRAN.R-project.org/package=vip}.
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
#' gbm_fit %>% get_vi
#' gbm_fit %>% get_vi(scale = TRUE)
#' }
#' @export
get_vi <- function(mfit, ...) mfit %>% vip::vi(...)
