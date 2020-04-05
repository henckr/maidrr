#' Get feature grid
#'
#' Get the grid values for a feature based on the observed values in the data.
#'
#' @param var Character string giving the name of the feature of interest. Only
#'   integer, numeric or factor variables are supported for now.
#' @param data Data frame containing the original training data.
#' @return One-column tidy data frame (i.e., a "tibble" object). This column
#'   contains the grid values for feature \code{var} based on the observed
#'   values in \code{data}.
#' @examples
#' \dontrun{
#' data('mtpl_be')
#' 'ageph' %>% get_grid(data = mtpl_be)
#' 'coverage' %>% get_grid(data = mtpl_be)
#' tidyr::expand_grid('ageph' %>% get_grid(data = mtpl_be),
#'                    'coverage' %>% get_grid(data = mtpl_be))
#' }
#' @export
get_grid <- function(var, data) {

  if (! var %in% names(data)) stop('The specified variable could not be found in the supplied data.')

  # Integer or numeric variable
  if (any(c('integer', 'numeric') %in% class(data[, var])))  return(tibble::tibble(sort(unique(data[, var]))) %>% setNames(var))
  # Factor or oredered variable
  if (any(c('factor', 'ordered') %in% class(data[, var]))) return(tibble::tibble(factor(levels(data[, var]), ordered = is.ordered(data[, var]))) %>% setNames(var))

  stop('Unsupported variable type. Only integers, numerics and factors are allowed.')

}
