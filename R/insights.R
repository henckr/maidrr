#' Model insights
#'
#' Obtain insights from a black box model in the form of feature effects.
#'
#' @param mfit A fitted model object (e.g., a "gbm" or "randomForest" object).
#' @param vars A character vector specifying the features to get insights on.
#' @param data Data frame containing the original training data.
#' @param interactions A string specifying how to deal with interaction effects:
#'   \describe{
#'   \item{'none'}{no interactions between features included in the output.}
#'   \item{'user'}{specify interactions in \code{vars} as \code{"var1_var2"}.}
#'   \item{'auto'}{automatic selection of interactions based on \code{hcut}.}
#'   }
#' @param hcut A numeric in the range \[0,1\] specifying the cut-off value for the
#'   normalized cumulative H-statistic over all two-way interactions, ordered from
#'   most to least important, between the features in \code{var}. Note that
#'   \code{hcut = 0} will add the single most important interaction, while
#'   \code{hcut = 1} will add all possible two-way interactions.
#' @param pred_fun Optional prediction function for the model in \code{mfit} that
#' requires two arguments: \code{object} and \code{newdata}.
#' @return A list of tidy data frames (i.e., "tibble" objects), each containing
#' feature effects for the features in \code{var}, possibly with interactions.
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
#'                      pred_fun = gbm_fun)
#' gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel'),
#'                      data = mtpl_be,
#'                      interactions = 'user',
#'                      pred_fun = gbm_fun)
#' }
#' @export
insights <- function(mfit, vars, data, interactions = 'auto', hcut = 0.5, pred_fun = NULL) {

  vars_main <- vars[! grepl('_', vars)]
  if (! all(vars_main %in% names(data))) stop('Some features specified in vars can not be found in the data.')

  if (interactions == 'none') {
    vars_intr <- character()
    if (length(vars[grepl('_', vars)]) > 0) warning('Interactions specified in vars are ignored when interactions = "none".')
  }
  if (interactions == 'auto') {
    vars_intr <- apply(combn(vars_main, 2), 2, function(x) paste(x, collapse = '_'))
    if (length(vars[grepl('_', vars)]) > 0) warning('Interactions specified in vars are ignored when interactions = "auto".')
    if (hcut < 0 | hcut > 1) stop('The parameter hcut must lie within the range [0, 1].')
  }
  if (interactions == 'user') {
    vars_intr <- vars[grepl('_', vars)]
    if (! all(unique(unlist(sapply(vars_intr, function(x) strsplit(x, '_')))) %in% vars_main)) stop('Each feature that is included in an interaction should also be present as a main effect.')
  }

  # Get the effects for all features
  vars <- c(vars_main, vars_intr)
  fx_vars <- setNames(vector('list', length = length(vars)), vars)
  for (v in vars) {
    fx_vars[[v]] <- get_pd(mfit = mfit,
                           var = v,
                           grid = switch(as.character(grepl('_', v)),
                                         'FALSE' = get_grid(v, data),
                                         'TRUE' = tidyr::expand_grid(get_grid(unlist(strsplit(v, '_'))[1], data), get_grid(unlist(strsplit(v, '_'))[2], data))),
                           data = data,
                           subsample = 10000,
                           fun = pred_fun)
  }

  # Determine which interactions to include
  if (interactions == 'auto') {
    vars_intr <- tibble::tibble(intr = vars_intr,
                                hstat = vars_intr %>% purrr::map_dbl(function(v) interaction_strength(fx_vars[[v]], fx_vars[vars_main]))) %>%
      dplyr::arrange(-hstat) %>%
      dplyr::mutate(nrank = cumsum(hstat)/sum(hstat)) %>%
      dplyr::slice(seq_len(sum(nrank < hcut) + 1)) %>%
      dplyr::pull(intr)

    fx_vars <- fx_vars[c(vars_main, vars_intr)]
  }

  # Get the pure interaction effects
  if( interactions %in% c('auto', 'user')) {
    fx_vars[vars_intr] <- fx_vars[vars_intr] %>% lapply(function(fx) interaction_pd(fx, fx_vars[vars_main]))
  }

  return(fx_vars)
}
