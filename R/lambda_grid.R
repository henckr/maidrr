#' Lambda grid
#'
#' Create a grid of lambda values that result in a unique grouping.
#'
#' @param fx_vars List of data frames containing the feature effects.
#' @param lambda_range Numeric vector of possible values for lambda.
#' @return Tidy data frame (i.e., a "tibble" object) with the lambda grid. The
#'   first column contains the lambda values from \code{lambda_range} which
#'   result in a unique grouping for the features in \code{fx_vars}. There is
#'   one column for each feature containing the optimal number of groups
#'   corresponding to those lambda values.
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
#'             lambda_grid
#' }
#' @export
lambda_grid <- function(fx_vars, lambda_range = as.vector(outer(seq(1, 10, 0.1), 10^(-7:3)))) {

  # Split the main and interaction effects
  vars <- unlist(lapply(fx_vars, comment))
  vars_main <- vars[! grepl('_', vars)]
  vars_intr <- vars[grepl('_', vars)]

  # Get lambdas that result in unique grouping for main effects
  if (length(vars_main) > 0) {
    grid_main <- tibble::tibble(lambda_main = lambda_range)
    for (v in vars_main) {
      grid_main <- grid_main %>% dplyr::mutate(!!v := purrr::map2_int(lambda_range, v, function(x, y) optimal_ngroups(fx_vars[[y]], x)))
    }
    grid_main <- grid_main %>% dplyr::distinct(!!! rlang::syms(setdiff(names(grid_main), 'lambda_main')), .keep_all = TRUE)
  }

  # Get lambdas that result in unique grouping for interaction effects
  if (length(vars_intr) > 0) {
    grid_intr <- tibble::tibble(lambda_intr = lambda_range)
    for (v in vars_intr) {
      grid_intr <- grid_intr %>% dplyr::mutate(!!v := purrr::map2_int(lambda_range, v, function(x, y) optimal_ngroups(fx_vars[[y]], x)))
    }
    grid_intr <- grid_intr %>% dplyr::distinct(!!! rlang::syms(setdiff(names(grid_intr), 'lambda_intr')), .keep_all = TRUE)
  }

  # Output the (combined) grid
  if (length(vars_main) > 0 & length(vars_intr) > 0) {
    return(tidyr::expand_grid(lambda_main = grid_main$lambda_main, lambda_intr = grid_intr$lambda_intr) %>%
             dplyr::left_join(grid_main, by = 'lambda_main') %>%
             dplyr::left_join(grid_intr, by = 'lambda_intr'))
  }
  if (length(vars_main) > 0) return(grid_main)
  if (length(vars_intr) > 0) return(grid_intr)
}
