#' Surrogate GLM
#'
#' Fit a surrogate generalized linear model (GLM) to the segmented data.
#'
#' @param data Data frame containing the segmented training data.
#' @param par_list Named list, constructed via \code{\link{alist}}, containing
#'   additional arguments to be passed on to \code{\link[stats]{glm}}. Examples
#'   are: \code{formula}, \code{family}, \code{weights} or \code{offset}.
#' @return GLM (i.e., a "glm" object) which is fit to the segmented \code{data}.
#' @seealso Extra info on arguments can be found at \code{\link[stats]{glm}}.
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
#'                          values = setNames(c(7, 8, 2, 2, 3), c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel'))) %>%
#'             surrogate(par_list = alist(formula = nclaims ~ ageph_ + bm_ + coverage_ + fuel_ + bm_fuel_,
#'                                        family =  poisson(link = 'log'),
#'                                        offset = log(expo)))
#' }
#' @export
surrogate <- function(data, par_list) {
  do.call('glm', c(list(data = quote(data)), par_list))
}
