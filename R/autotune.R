#' Automatic tuning
#'
#' Automated tuning process for the penalty parameter lambda, with built-in
#' feature selection. Lambda directly influences the granularity of the
#' segmentation, with low/high values resulting in a fine/coarse segmentation.
#'
#' @param mfit Fitted model object (e.g., a "gbm" or "randomForest" object).
#' @param data Data frame containing the original training data.
#' @param vars Character vector specifying the features in \code{data} to use.
#' @param target String specifying the target (or response) variable to model.
#' @param hcut Numeric in the range \[0,1\] specifying the cut-off value for the
#'   normalized cumulative H-statistic over all two-way interactions, ordered
#'   from most to least important, between the features in \code{vars}. Note
#'   that \code{hcut = 0} will consider the single most important interaction,
#'   while \code{hcut = 1} will consider all possible two-way interactions.
#'   Setting \code{hcut = -1} will only consider main effects in the tuning.
#' @param ignr_intr Optional character string specifying features to ignore when
#'   searching for meaningful interactions to incorporate in the GLM.
#' @param pred_fun Optional prediction function to calculate feature effects for
#'   the model in \code{mfit}. Requires two arguments: \code{object} and
#'   \code{newdata}. See \code{\link[pdp:partial]{pdp::partial}} and this
#'   \href{https://bgreenwell.github.io/pdp/articles/pdp-extending.html}{article}
#'    for the details. See also the function \code{gbm_fun} in the example.
#' @param lambdas Numeric vector with the possible lambda values to explore. The
#'   search grid is created automatically via \code{\link{lambda_grid}} such
#'   that it contains only those values of lambda that result in a unique
#'   grouping of the full set of features. A seperate grid is generated for main
#'   and interaction effects, due to the scale difference in both types.
#' @param nfolds Integer for the number of folds in K-fold cross-validation.
#' @param strat_vars Character (vector) specifying the feature(s) to use for
#'   stratified sampling. The default NULL implies no stratification is applied.
#' @param glm_par Named list, constructed via \code{\link{alist}}, containing
#'   arguments to be passed on to \code{\link[stats]{glm}}. Examples are:
#'   \code{family}, \code{weights} or \code{offset}. Note that \code{formula}
#'   will be ignored as the GLM formula is determined by the specified
#'   \code{target} and the automatic feature selection in the tuning process.
#' @param err_fun Error function to calculate the prediction errors on the
#'   validation folds. This must be an R function which outputs a single number
#'   and takes two vectors \code{y_pred} and \code{y_true} as input for the
#'   predicted and true target values respectively. An additional input vector
#'   \code{w_case} is allowed to use case weights in the error function. The
#'   weights are determined automatically based on the \code{weights} field
#'   supplied to \code{glm_par}. Examples already included in the package are:
#'   \describe{ \item{mse}{mean squared error loss function (default).}
#'   \item{wgt_mse}{weighted mean squared error loss function.}
#'   \item{poi_dev}{Poisson deviance loss function.} } See \code{\link{err_fun}}
#'   for details on these predefined functions.
#' @param ncores Integer specifying the number of cores to use. The default
#'   \code{ncores = -1} uses all the available physical cores (not threads), as
#'   determined by \code{parallel::detectCores(logical = 'FALSE')}.
#' @param out_pds Boolean to indicate whether to add the calculated PD effects
#'   for the selected features to the output list.
#' @return List with the following elements: \describe{ \item{slct_feat}{named
#'   vector containing the selected features (names) and the optimal number of
#'   groups for each feature (values).} \item{best_surr}{the optimal GLM
#'   surrogate, which is fit to all observations in \code{data}. The segmented
#'   data can be obtained via the \code{$data} attribute of the GLM fit.}
#'   \item{tune_main}{ the cross-validation results for the main effects as a
#'   tidy data frame. The column \code{cv_err} contains the cross-validated
#'   error, while the columns \code{1:nfolds} contain the error on the
#'   validation folds.} \item{tune_intr}{cross-validation results for the
#'   interaction effects.} \item{pd_fx}{List with the PD effects for the
#'   selected features (only present if \code{out_pds = TRUE}).}}
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
#' gbm_fit %>% autotune(data = mtpl_be,
#'                      vars = c('ageph', 'bm', 'coverage', 'fuel', 'sex', 'fleet', 'use'),
#'                      target = 'nclaims',
#'                      hcut = 0.75,
#'                      pred_fun = gbm_fun,
#'                      lambdas = as.vector(outer(seq(1, 10, 1), 10^(-6:-2))),
#'                      nfolds = 5,
#'                      strat_vars = c('nclaims', 'expo'),
#'                      glm_par = alist(family = poisson(link = 'log'),
#'                                      offset = log(expo)),
#'                      err_fun = poi_dev,
#'                      ncores = -1)
#' }
#' @export
autotune <- function(mfit, data, vars, target, hcut = 0.75, ignr_intr = NULL, pred_fun = NULL, lambdas = as.vector(outer(seq(1, 10, 0.1), 10^(-7:3))), nfolds = 5, strat_vars = NULL, glm_par = alist(), err_fun = mse, ncores = -1, out_pds = FALSE) {

  if (sum(grepl('_', vars)) > 0) stop('No underscores allowed in the variable names, these are interpreted as interactions in maidrr.')
  if (! all(vars %in% names(data))) stop('All the variables needs to be present in the data.')
  if (! target %in% names(data)) stop('The target variable needs to be present in the data.')

  if (! ((hcut == -1) | (0 <= hcut & hcut <= 1))) stop('Invalid value specified for hcut, must equal -1 or lie within the range [0, 1].')

  if (nfolds <= 1) stop('At least two folds are needed to perform K-fold cross-validation.')

  if ((! is.null(strat_vars)) & (! all(strat_vars %in% names(data)))) stop('The stratification variables in strat_vars need to be present in the data.')

  if ('formula' %in% names(glm_par)) {
    warning('The field "formula" is removed from glm_par as this is determined by the supplied target and automatic feature selection.')
    glm_par[['formula']] <- NULL
  }

  if(! all(paste0('y_', c('true', 'pred')) %in% names(formals(err_fun)))) stop('The error function must contain arguments y_true and y_pred.')
  if (length(setdiff(names(formals(err_fun)), c('y_true', 'y_pred', 'w_case'))) > 0) stop('The error function can only contain arguments y_true, y_pred and w_case.')
  if (('w_case' %in% names(formals(err_fun))) & (! 'weights' %in% names(glm_par))) stop('If w_case is an argument in err_fun, weights must be an argument in glm_par.')

  if (ncores < -1 | ncores == 0) stop('The number of cores must be strictly positive, or equal to -1 for all available cores.')
  if (ncores > parallel::detectCores()) warning('The asked number of cores is larger than parallel::detectCores, so there might be trouble ahead.')

  # Function to calculate validation errors
  Kfold_cross_valid <- function(data, target, nfolds, glm_par, err_fun, fixed_terms = NULL){
    val_err <- rep(NA, nfolds)
    for (f in seq_len(nfolds)) {
      # Get training fold
      trn_data <- dplyr::filter(data, fold != f)
      # Feature selection (only keep those with >1 group) and determine the GLM formula
      slct_feat <- names(which(unlist(lapply(trn_data[, grepl('_$', names(trn_data))], function(x) length(unique(x)) > 1))))
      glm_par[['formula']] <- as.formula(paste(target, '~', paste(c(1, fixed_terms, slct_feat), collapse = ' + ')))
      # Fit surrogate GLM on the training fold
      glm_fit <- maidrr::surrogate(trn_data, glm_par)
      # Get validation fold and calculate the prediction error
      val_data <- maidrr::rm_lvls(glm_fit, dplyr::filter(data, fold == f))
      y_pred <- suppressWarnings(predict(glm_fit, newdata = val_data, type = 'response'))
      y_true <- dplyr::pull(val_data, target)
      w_case <- val_data[[deparse(glm_fit$call$weights)]]
      val_err[f] <- ifelse(length(glm_fit$coefficients) > glm_fit$rank, Inf,
                           do.call(err_fun, setNames(lapply(names(formals(err_fun)), function(x) eval(as.name(x))), names(formals(err_fun)))))
    }
    return(val_err)
  }

  # Create the fold indicator via (stratified) sampling
  data <- data %>% dplyr::sample_n(size = nrow(.), replace = FALSE) %>%
    dplyr::arrange(!!! rlang::syms(strat_vars)) %>%
    dplyr::mutate(fold = rep_len(seq_len(nfolds), nrow(.)))

  # Generate the feature effects and the lambda grid for main effects
  fx_main <- maidrr::insights(mfit = mfit,
                              vars = vars,
                              data = data,
                              interactions = 'user',
                              pred_fun = pred_fun)
  lmbd_main <- maidrr::lambda_grid(fx_vars = fx_main, lambda_range = lambdas)

  # Set up a cluster and iterate over the lambda grid
  switch(as.character(ncores),
         '-1' = doParallel::registerDoParallel(parallel::detectCores(logical = 'FALSE')),
         '1' = foreach::registerDoSEQ(),
         doParallel::registerDoParallel(ncores))
  out_main <- foreach::foreach (l = seq_len(nrow(lmbd_main)), .combine = rbind) %dopar% {
    # Segmentation for current lambda values
    data_segm <- maidrr::segmentation(fx_vars = fx_main, data = data, type = 'ngroups',
                                      values =  unlist(dplyr::select(dplyr::slice(lmbd_main, l), vars), use.names = TRUE))
    # Calculate the validation errors
    Kfold_cross_valid(data = data_segm, target = target, nfolds = nfolds, glm_par = glm_par, err_fun = err_fun)
  }
  doParallel::stopImplicitCluster()

  # Get the cross-validation error
  out_main <- dplyr::bind_cols(lmbd_main, out_main %>% as.data.frame %>% setNames(seq_len(nfolds))) %>%
    dplyr::mutate(cv_err = rowMeans(dplyr::select(., as.character(seq_len(nfolds))), na.rm = TRUE)) %>% dplyr::arrange(cv_err)

  # Get the selected features
  slct_main <- out_main %>% dplyr::slice(1) %>% dplyr::select(!!!rlang::syms(vars)) %>%
    dplyr::select_if(~. > 1) %>% unlist(., use.names = TRUE)
  if (length(slct_main) == 0) stop('Not a single feature was selected, please try again with lower values for lambda.')

  # Segment the data based on the selected features and optimal number of groups
  data_main <- maidrr::segmentation(fx_vars = fx_main[names(slct_main)], data = data, type = 'ngroups', values = slct_main)


  # Return these results if no interactions are asked for
  if (hcut == -1) {
    # Fit the optimal surrogate GLM
    glm_par[['formula']] <- as.formula(paste(target, '~', paste(c(1, paste0(names(slct_main), '_')), collapse = ' + ')))
    opt_surro <- maidrr::surrogate(data = data_main, par_list = glm_par)

    # Combine results in a list and return
    output <- list('slct_feat' = slct_main,
                   'best_surr' = opt_surro,
                   'tune_main' = out_main,
                   'tune_intr' = NULL)
    # Add PD effects if asked for
    if (out_pds) output$pd_fx <- fx_main[names(slct_main)]
    # Output
    return(output)
  }


  # Generate the feature effects and the lambda grid for interactions
  fx_intr <- maidrr::insights(mfit = mfit,
                              vars = setdiff(names(slct_main), ignr_intr),
                              data = data,
                              interactions = 'auto',
                              hcut = hcut,
                              pred_fun = pred_fun,
                              fx_in = fx_main)
  fx_intr <- fx_intr[grepl('_', names(fx_intr))]
  vars_intr <- names(fx_intr)
  lmbd_intr <- maidrr::lambda_grid(fx_vars = fx_intr, lambda_range = lambdas)

  # Set up a cluster and iterate over the lambda grid
  switch(as.character(ncores),
         '-1' = doParallel::registerDoParallel(parallel::detectCores(logical = 'FALSE')),
         '1' = foreach::registerDoSEQ(),
         doParallel::registerDoParallel(ncores))
  out_intr <- foreach::foreach (l = seq_len(nrow(lmbd_intr)), .combine = rbind) %dopar% {
    # Segmentation for current lambda values
    data_segm <- maidrr::segmentation(fx_vars = fx_intr, data = data_main, type = 'ngroups',
                                      values =  unlist(dplyr::select(dplyr::slice(lmbd_intr, l), vars_intr), use.names = TRUE))
    # Calculate the validation errors
    Kfold_cross_valid(data = data_segm, target = target, nfolds = nfolds, glm_par = glm_par, err_fun = err_fun, fixed_terms = paste0(names(slct_main), '_'))
  }
  doParallel::stopImplicitCluster()

  # Get the cross-validation error
  out_intr <- dplyr::bind_cols(lmbd_intr, out_intr %>% as.data.frame %>% setNames(seq_len(nfolds))) %>%
    dplyr::mutate(cv_err = rowMeans(dplyr::select(., as.character(seq_len(nfolds))), na.rm = TRUE)) %>% dplyr::arrange(cv_err)
  if (all(out_intr$cv_err == Inf)) warning('All interaction combinations lead to rank-deficient GLM fits, try adding larger values for lambda to the grid.')

  # Get the selected features
  slct_intr <- out_intr %>% dplyr::slice(1) %>% dplyr::select(!!!rlang::syms(vars_intr)) %>%
    dplyr::select_if(~. > 1) %>% unlist(., use.names = TRUE)

  # Check if interactions improve the fit with only main effects
  if (min(out_intr$cv_err) < min(out_main$cv_err)) slct_feat <- c(slct_main, slct_intr) else slct_feat <- slct_main

  # Segment the data based on the selected features and optimal number of groups
  data_segm <- maidrr::segmentation(fx_vars = c(fx_main, fx_intr)[names(slct_feat)], data = data, type = 'ngroups', values = slct_feat)

  # Fit the optimal surrogate GLM
  glm_par[['formula']] <- as.formula(paste(target, '~', paste(c(1, paste0(names(slct_feat), '_')), collapse = ' + ')))
  opt_surro <- maidrr::surrogate(data = data_segm, par_list = glm_par)

  # Combine results in a list and return
  output <- list('slct_feat' = slct_feat,
                 'best_surr' = opt_surro,
                 'tune_main' = out_main,
                 'tune_intr' = out_intr)
  # Add PD effects if asked for
  if (out_pds) output$pd_fx <- c(fx_main, fx_intr)[names(slct_feat)]
  # Output
  return(output)
}
