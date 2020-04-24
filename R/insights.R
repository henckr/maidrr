#' Model insights
#'
#' Obtain insights from a black box model in the form of feature effects.
#'
#' @param mfit Fitted model object (e.g., a "gbm" or "randomForest" object).
#' @param vars Character vector specifying the features to get insights on.
#' @param data Data frame containing the original training data.
#' @param interactions String specifying how to deal with interaction effects:
#'   \describe{ \item{'user'}{specify interactions in \code{vars} as
#'   \code{"var1_var2"}.} \item{'auto'}{automatic selection of interactions
#'   based on \code{hcut}.} }
#' @param hcut Numeric in the range \[0,1\] specifying the cut-off value for the
#'   normalized cumulative H-statistic over all two-way interactions, ordered
#'   from most to least important, between the features in \code{vars}. Note
#'   that \code{hcut = 0} will add the single most important interaction, while
#'   \code{hcut = 1} will add all possible two-way interactions.
#' @param pred_fun Optional prediction function to calculate feature effects for
#'   the model in \code{mfit}. Requires two arguments: \code{object} and
#'   \code{newdata}. See \code{\link[pdp:partial]{pdp::partial}} and this
#'   \href{https://bgreenwell.github.io/pdp/articles/pdp-extending.html}{article}
#'    for the details. See also the function \code{gbm_fun} in the example.
#' @param fx_in Optional named list of data frames containing feature effects
#'   for features in \code{vars} that are already calculated beforehand, to
#'   avoid having to calculate these again. A possible use case is to supply the
#'   main effects such that only the interaction effects still need to be
#'   calculated. Precalculated interactions are ignored when \code{interactions
#'   = "auto"}, but can be supplied when \code{interactions = "user"}. It is
#'   important to make sure that you supply the pure interaction effects.
#' @param ncores Integer specifying the number of cores to use. The default
#'   \code{ncores = -1} uses all the available physical cores (not threads), as
#'   determined by \code{parallel::detectCores(logical = 'FALSE')}.
#' @return List of tidy data frames (i.e., "tibble" objects), containing the
#'   partial dependencies for the features (and interactions) in \code{vars}.
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
#' gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel'),
#'                      data = mtpl_be,
#'                      interactions = 'auto',
#'                      hcut = 0.75,
#'                      pred_fun = gbm_fun)
#' gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel'),
#'                      data = mtpl_be,
#'                      interactions = 'user',
#'                      pred_fun = gbm_fun)
#' }
#' @export
insights <- function(mfit, vars, data, interactions = 'user', hcut = 0.75, pred_fun = NULL, fx_in = NULL, ncores = -1) {

  vars_main <- vars[! grepl('_', vars)]
  if (! all(vars_main %in% names(data))) stop('Some features specified in vars can not be found in the data.')

  if (interactions == 'user') {
    vars_intr <- vars[grepl('_', vars)]
    if (! all(unique(unlist(sapply(vars_intr, function(x) strsplit(x, '_')))) %in% vars_main)) stop('Each feature that is included in an interaction should also be present as a main effect.')
  }

  if (interactions == 'auto') {
    if (length(vars[grepl('_', vars)]) > 0) warning('Interactions specified in vars are ignored when interactions = "auto".')
    vars_intr <- apply(combn(vars_main, 2), 2, function(x) paste(x, collapse = '_'))
    if (length(fx_in[grepl('_', names(fx_in))]) > 0) warning('Interactions specified in fx_in are ignored when interactions = "auto".')
    fx_in <- fx_in[! grepl('_', names(fx_in))]
    if (hcut < 0 | hcut > 1) stop('The parameter hcut must lie within the range [0, 1].')
  }


  # Initialize a list to save the results and possibly fill with supplied effects
  vars <- c(vars_main, vars_intr)
  fx_vars <- setNames(vector('list', length = length(vars)), vars)
  fx_vars[names(fx_in)] <- fx_in

  # Set up a cluster
  switch(as.character(ncores),
         '-1' = doParallel::registerDoParallel(parallel::detectCores(logical = 'FALSE')),
         '1' = foreach::registerDoSEQ(),
         doParallel::registerDoParallel(ncores))

  # Iterate over the variables to get effects
  fx_vars[setdiff(vars, names(fx_in))] <- foreach::foreach (v = setdiff(vars, names(fx_in))) %dopar% {
    maidrr::get_pd(mfit = mfit,
                   var = v,
                   grid = switch(as.character(grepl('_', v)),
                                 'FALSE' = maidrr::get_grid(v, data),
                                 'TRUE' = tidyr::expand_grid(maidrr::get_grid(unlist(strsplit(v, '_'))[1], data), maidrr::get_grid(unlist(strsplit(v, '_'))[2], data))),
                   data = data,
                   subsample = 10000,
                   fun = pred_fun)
  }
  doParallel::stopImplicitCluster()

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
  if( length(vars_intr) > 0) {
    fx_vars[setdiff(vars_intr, names(fx_in))] <- fx_vars[setdiff(vars_intr, names(fx_in))] %>% lapply(function(fx) interaction_pd(fx, fx_vars[vars_main]))
  }

  return(fx_vars)
}
