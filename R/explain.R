#' Explain predictions
#'
#' Explain a prediction of the surrogate GLM via each feature's contribution.
#'
#' @param surro The surrogate GLM fit (i.e., a "glm" object).
#' @param instance Single row data frame with the instance to be explained.
#' @param plt Boolean whether to return a ggplot or the underlying data.
#' @return Tidy data frame or ggplot with each feature's contribution to the
#'   prediction of model \code{surro} on observation \code{instance}. When
#'   \code{plt = FALSE}, the columns \code{fit_link} and \code{se_link} contain
#'   the fitted coefficient and standard error on the linear predictor scale.
#'   The column \code{fit_resp} contains the coefficient on the response scale
#'   after taking the inverse link function. The columns \code{upr_conf} and
#'   \code{lwr_conf} contain the upper and lower bound of a 95\% confidence
#'   interval on the response scale. When \code{plt = TRUE} the ggplot shows the
#'   coefficient and confidence interval on the response scale. A green dashed
#'   line shows the value of the invere link function applied to zero. Features
#'   with bars close to this line have a neglegible impact on the predition.
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
#' data_segm <- gbm_fit %>% insights(vars = c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel'),
#'                                   data = mtpl_be,
#'                                   interactions = 'user',
#'                                   pred_fun = gbm_fun) %>%
#'                           segmentation(data = mtpl_be,
#'                                        type = 'ngroups',
#'                                        values = setNames(c(7, 8, 2, 2, 3), c('ageph', 'bm', 'coverage', 'fuel', 'bm_fuel')))
#' data_segm %>% surrogate(formula = nclaims ~ ageph_ + bm_ + coverage_ + fuel_ + bm_fuel_,
#'                         family =  poisson(link = 'log'),
#'                         offset = log(expo)) %>%
#'               explain(instance = data_segm[34, ])
#' }
#' @export
explain <- function(surro, instance, plt = TRUE) {

  if (nrow(instance) > 1) stop('Can only explain one instance prediction at a time.')

  # Get the predictions for each term on the linear predictor scale
  preds <- surro %>% predict(newdata = instance, type = 'terms', se.fit = TRUE)

  # Get the inverse link function
  ilink_fun <- surro$family$linkinv

  # Calculate the fit and 95% confidence bounds
  coefs <- tibble::tibble(term = sub('_$', '', names(preds$fit[1, ])),
                          value = term,
                          fit_link = preds$fit[1, ],
                          se_link = preds$se.fit[1, ],
                          fit_resp = ilink_fun(fit_link),
                          upr_conf = ilink_fun(fit_link + (2 * se_link)),
                          lwr_conf = ilink_fun(fit_link - (2 * se_link)))

  # Show feature values for the main effects
  instance_chr <- instance %>% dplyr::mutate_if(is.factor, as.character)
  coefs[! grepl('_', coefs$term), 'value'] <-  coefs[! grepl('_', coefs$term), ][['term']] %>% sprintf('%s=%s', ., instance_chr[.])

  if (plt) return(coefs %>% ggplot(aes(x = reorder(value, -fit_resp), y = fit_resp)) +
                    geom_bar(position = 'identity', stat = 'identity', fill = '#999999') +
                    geom_errorbar(aes(ymin = lwr_conf, ymax = upr_conf), width = 0.5) +
                    geom_hline(yintercept = ilink_fun(0), color = 'darkgreen', size = 1, linetype = 'dashed') +
                    coord_flip() + labs(x = '', y = 'Feature contributions') + theme_bw())

  return(coefs)
}
