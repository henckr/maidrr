#' Surrogate GLM
#'
#' Fit a surrogate generalized linear model (GLM) to the segmented data.
#'
#' @param data Data frame containing the segmented training data.
#' @param par_list A named list, constructed via \code{\link{alist}}, containing
#'   additional arguments to be passed on to \code{\link[stats]{glm}}. Examples
#'   are: \code{formula}, \code{family}, \code{weights} or \code{offset}.
#' @return A GLM (i.e., a "glm" object) which is fit to the segmented data.
#' @seealso Extra info on arguments at \code{\link[stats]{glm}}.
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
#'                          type = 'lambdas',
#'                          values = 0.0001) %>%
#'             surrogate(par_list = alist(formula = nclaims ~ ageph_ + power_,
#'                                        family =  poisson(link = 'log'),
#'                                        offset = log(expo)))
#' }
#' @export
surrogate <- function(data, par_list) {
  do.call('glm', c(list(data = quote(data)), par_list))
}
