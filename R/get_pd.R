#' Calculate partial dependence
#'
#' Compute partial dependence functions (i.e., marginal effects) for the
#' predictors in a model. Note that \code{get_pd} is based on
#' \code{\link[pdp:partial]{pdp::partial}} and adds observation weights.
#'
#' @param mfit Fitted model object (e.g., a "gbm" or "randomForest" object).
#' @param var Character string giving the name of the predictor variable of
#'   interest. For an interaction effect, specify as \code{"var1_var2"}. For now
#'   only two-way interactions are supported, so do not specify more than two
#'   variable names (i.e., only one underscore allowed in the string).
#' @param grid Data frame containing the (joint) values of interest for the
#'   feature(s) listed in \code{var}. One column for main effects and two
#'   columns for an interaction effect, with feature names as the column names.
#'   See the documentation and examples of \code{\link{get_grid}} for details.
#' @param data Data frame containing the original training data.
#' @param subsample Optional integer specifying the number of observations to
#'   use for the computation of the partial dependencies. Defaults to the number
#'   of observations in \code{data}, but a smaller value saves computation time.
#' @param pred_fun Optional prediction function to calculate feature effects for
#'   the model in \code{mfit}. Requires two arguments: \code{object} and
#'   \code{newdata}. See \code{\link[pdp:partial]{pdp::partial}} and this
#'   \href{https://bgreenwell.github.io/pdp/articles/pdp-extending.html}{article}
#'    for the details. See also the function \code{gbm_fun} in the example.
#' @param ... Additional optional arguments to be passed onto
#'   \code{\link[pdp:partial]{pdp::partial}}.
#' @return Tidy data frame (i.e., a "tibble" object) with three (x, y, w) or
#'   four (x1, x2, y, w) columns for respectively a main and two-way interaction
#'   effect. Column(s) \code{x} contain variable values, column \code{y} the
#'   partial dependence effect and \code{w} the observation counts in
#'   \code{data}. The data frame attribute \code{comment} contains the variable
#'   name, as specified in the \code{var} argument.
#' @seealso \code{\link[pdp]{partial}} and
#'   \url{https://CRAN.R-project.org/package=pdp}.
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
#' gbm_fit %>% get_pd(var = 'ageph',
#'                    grid = 'ageph' %>% get_grid(data = mtpl_be),
#'                    data = mtpl_be,
#'                    subsample = 10000,
#'                    fun = gbm_fun)
#' gbm_fit %>% get_pd(var = 'power_coverage',
#'                    grid = tidyr::expand_grid('ageph' %>% get_grid(data = mtpl_be),
#'                                              'coverage' %>% get_grid(data = mtpl_be)),
#'                    data = mtpl_be,
#'                    subsample = 10000,
#'                    fun = gbm_fun)
#' }
#' @export
get_pd <- function(mfit, var, grid, data, subsample = nrow(data), fun = NULL, ...) {

  if (length(var) > 1) stop('Specify interactions via the underscore, not via a character vector.')
  if (length(unlist(strsplit(var, '_'))) > 2) stop('Only two-way interactions are supported for now.')

  # Compute the partial dependence
  pd <- mfit %>% pdp::partial(pred.var = unlist(strsplit(var, '_')),
                              pred.grid = grid,
                              pred.fun = fun,
                              train = data[sort(sample(seq_len(nrow(data)), size = subsample, replace = (subsample > nrow(data)))), ],
                              ice = FALSE,
                              plot = FALSE,
                              recursive = is.null(fun),
                              ...)

  # Merge with the observation counts
  pd <- merge(pd, data %>% dplyr::count(!!!rlang::syms(as.list(strsplit(var, '_')[[1]]))),
              by = unlist(strsplit(var, '_')), all.x = TRUE)

  # Standardize column names and add comment attribute with variable name(s)
  names(pd) <- switch(as.character(ncol(pd)),
                      '3' = c('x','y','w'),
                      '4' = c('x1','x2','y','w'))
  comment(pd) <- var

  # Replace NAs and output
  pd %>% tidyr::replace_na(list(w = 0)) %>% tibble::as_tibble()

}
