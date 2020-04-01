#' Automatic tuning
#'
#' Automated tuning process for the penalty parameter lambda, with built-in
#' feature selection. Lambda directly influences the granularity of the
#' segmentation, with low/high values resulting in a fine/coarse segmentation.
#'
#' @param fx_vars A list of data frames containing the feature effects. These
#'   can be obtained by applying \code{\link{insights}} on your model fit.
#' @param data Data frame containing the original training data.
#' @param target A string specifying the target (or response) variable to model.
#' @param lambdas A numeric vector with the possible lambda values to explore.
#'   The search grid is created automatically via \code{\link{lambda_grid}}.
#'   This grid contains only those values of lambda that result in a unique
#'   grouping of the full set of features. If both main and interaction effects
#'   are present in \code{fx_vars}, a seperate grid is generated and all the
#'   pairwise combinations are evaluated in a cartesian grid search.
#' @param nfolds An integer for the number of folds in K-fold cross-validation.
#' @param strat_vars A character (vector) specifying the feature(s) to use for
#'   stratified sampling. The default NULL implies no stratification is applied.
#' @param glm_par A named list, constructed via \code{\link{alist}}, containing
#'   arguments to be passed on to \code{\link[stats]{glm}}. Examples are:
#'   \code{family}, \code{weights} or \code{offset}. Note that \code{formula}
#'   will be ignored as the GLM formula is determined by the specified
#'   \code{target} and the automatic feature selection in the tuning process.
#' @param err_fun An error function to calculate the prediction errors on the
#'   validation folds. This must be an R function which outputs a single number
#'   and takes two vectors \code{y_pred} and \code{y_true} as input for the
#'   predicted and true target values respectively. An additional input vector
#'   \code{w_case} is allowed to use case weights in the error function. The
#'   weights are determined automatically based on the \code{weights} field
#'   supplied to \code{glm_par}. Examples already included in the package:
#'   \describe{ \item{mse}{mean squared error loss function.}
#'   \item{wgt_mse}{weighted mean squared error loss function.}
#'   \item{poi_dev}{Poisson deviance loss function.} }
#' @param ncores An integer specifying the number of cores to use. The default
#'   \code{ncores = -1} uses all the available physical cores (not threads), as
#'   determined by \code{parallel::detectCores(logical = 'FALSE')}.
#' @return A tidy data frame (i.e., a "tibble" object) with the cross-validation
#'   results. The column \code{cv_err} contains the cross-validated error, while
#'   the columns \code{1:nfolds} contain the error on the validation folds.
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
#'             autotune(data = mtpl_be,
#'                      target = 'nclaims',
#'                      lambdas = as.vector(outer(seq(1, 10, 1), 10^(-3:-1))),
#'                      nfolds = 5,
#'                      strat_vars = c('nclaims', 'expo'),
#'                      glm_par = alist(family = poisson(link = 'log'),
#'                                      offset = log(expo)),
#'                      err_fun = poi_dev,
#'                      ncores = -1)
#' }
#' @export
autotune <- function(fx_vars, data, target, lambdas = as.vector(outer(seq(1, 10, 0.1), 10^(-7:3))), nfolds = 5, strat_vars = NULL, glm_par = alist(), err_fun = mse, ncores = -1) {

  if (! target %in% names(data)) stop('The target variable needs to be present in the data.')

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

  # Create the grid of lambda values
  lmbd_grid <- maidrr::lambda_grid(fx_vars = fx_vars, lambda_range = lambdas)

  # Create the fold indicator via (stratified) sampling
  data <- data %>% dplyr::sample_n(size = nrow(.), replace = FALSE) %>%
                   dplyr::arrange(!!! rlang::syms(strat_vars)) %>%
                   dplyr::mutate(fold = rep_len(seq_len(nfolds), nrow(.)))

  switch(as.character(ncores),
         '-1' = doParallel::registerDoParallel(parallel::detectCores(logical = 'FALSE')),
         '1' = foreach::registerDoSEQ(),
         doParallel::registerDoParallel(ncores))

  # Iterate over the lambda grid
  out <- foreach::foreach (l = seq_len(nrow(lmbd_grid)), .combine = rbind) %dopar% {

    # Segmentation for current lambda values
    data_segm <- data
    for (i in seq_len(length(fx_vars))) {
      fx_var <- fx_vars[[i]]
      var <- fx_var %>% comment
      fx_grp <- fx_var %>% maidrr::group_pd(ngroups = lmbd_grid[[l, var]])

      data_segm <- data_segm %>% dplyr::left_join(fx_grp[c(paste0('x', if (grepl('_', var)) 1:2), 'xgrp')],
                                                  by = setNames(paste0('x', if (grepl('_', var)) 1:2), unlist(strsplit(var, '_')))) %>%
        dplyr::mutate(xgrp = relevel(as.factor(xgrp), ref =  (fx_grp %>% dplyr::arrange(-wgrp) %>% dplyr::pull(xgrp))[1])) %>%
        dplyr::rename(!!paste0(var, '_') := xgrp)
    }

    # Calculate the validation errors
    val_err <- rep(NA, nfolds)
    for (f in seq_len(nfolds)) {
      # Get training fold
      trn_data <- dplyr::filter(data_segm, fold != f)
      # Feature selection (only keep those with >1 group) and determine the GLM formula
      slct_feat <- names(which(unlist(lapply(trn_data[, grepl('_$', names(trn_data))], function(x) length(unique(x)) > 1))))
      glm_par[['formula']] <- as.formula(paste(target, '~', paste(c(1, slct_feat), collapse = ' + ')))
      # Fit surrogate GLM on the training fold
      glm_fit <- maidrr::surrogate(trn_data, glm_par)
      # Get validation fold and calculate the prediction error
      val_data <- maidrr::rm_lvls(glm_fit, dplyr::filter(data_segm, fold == f))
      y_pred <- predict(glm_fit, newdata = val_data, type = 'response')
      y_true <- dplyr::pull(val_data, target)
      w_case <- val_data[[deparse(glm_fit$call$weights)]]
      val_err[f] <- do.call(err_fun, setNames(lapply(names(formals(err_fun)), function(x) eval(as.name(x))), names(formals(err_fun))))
    }
    val_err
  }
  doParallel::stopImplicitCluster()

  dplyr::bind_cols(lmbd_grid, out %>% as.data.frame %>% setNames(seq_len(nfolds))) %>%
    dplyr::mutate(cv_err = rowMeans(dplyr::select(., as.character(seq_len(nfolds))), na.rm = TRUE)) %>%
    dplyr::arrange(cv_err)
}
