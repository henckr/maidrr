#' Get feature grid
#'
#' Get the grid values for features based on the observed values in the data.
#'
#' @param var Character string or vector giving the names of the features.
#' @param data Data frame containing the original training data.
#' @return Tidy data frame (i.e., a "tibble" object). The columns contain the grid
#' values for features \code{var} based on the observed values in \code{data}.
#' @examples
#' \dontrun{
#' data('mtpl_be')
#' 'ageph' %>% get_grid(data = mtpl_be)
#' 'coverage' %>% get_grid(data = mtpl_be)
#' c('ageph', 'coverage') %>% get_grid(data = mtpl_be)
#' tidyr::expand_grid('ageph' %>% get_grid(data = mtpl_be),
#'                    'coverage' %>% get_grid(data = mtpl_be))
#' }
#' @export
get_grid <- function(var, data) {

  if (! all(var %in% names(data))) stop(paste0('The following variable(s) could not be found in the supplied data: ', paste(var[! var %in% names(data)], collapse = ' ')))

  data %>% dplyr::select(!!!rlang::syms(var)) %>% dplyr::distinct() %>% dplyr::arrange(!!!rlang::syms(var)) %>% tibble::as_tibble()
}
