#' Plot partial dependence
#'
#' Plot the partial dependence functions (i.e., marginal effects) for the
#' predictors in a model.
#'
#' @param pd Data frame containing the partial dependence effect as returned by
#'   \code{\link{get_pd}}.
#' @return ggplot object showing the partial dependence effect in \code{pd}.
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
#'                    fun = gbm_fun) %>%
#'             plot_pd
#' }
#' @export
plot_pd <- function(pd) {

  var <- unlist(strsplit(comment(pd), '_'))

  # Check whether to plot the basic PD or grouped variant
  grp <- dplyr::is.grouped_df(pd)
  ngroups <- ifelse(grp, nrow(attr(pd, 'groups')), 1)
  plt_grp <- grp & ngroups > 1

  # Marginal effect
  if (length(var) == 1) {
    # Continuous variable
    if (any(c('integer', 'numeric') %in% class(pd$x))) {
      if (plt_grp) bndry <- sort(pd %>% dplyr::summarize(xmin = min(x)) %>% dplyr::pull(xmin))[2:ngroups] -  median(diff(pd$x))/2
      return(ggplot(pd, aes(x = x)) + geom_line(aes(y = y, color = -w), size = 1.5) +
               {if (plt_grp) geom_vline(xintercept = bndry, alpha = 0.75)} +
               theme_bw() + labs(x = var, y = 'partial dependence') + theme(legend.position = 'none'))
    }
    # Factor variable
    if (any(c('factor', 'ordered') %in% class(pd$x))) {
      return(ggplot(pd, aes(x = reorder(x, -y))) +
               {if (! plt_grp) geom_point(aes(y = y, size = w))} +
               {if (plt_grp) geom_point(aes(y = y, size = w, shape = as.factor(xgrp)))} +
               theme_bw() + labs(x = var, y = 'partial dependence') + theme(legend.position = 'none') +
               {if (plt_grp) scale_shape_manual(values = seq(0,14))})
    }
  }

  # Two-way interaction effect
  if (length(var) == 2) {
    # Two continuous variables
    if (all(sapply(pd[c('x1', 'x2')], function(x) class(x)[1]) %in% c('integer', 'numeric'))) {
      return(ggplot(pd, aes(x1, x2)) +
               {if (! plt_grp) geom_tile(aes(fill = y))} + # alpha = w
               {if (plt_grp) geom_tile(aes(fill = factor(round(ygrp, digits = 3))))} + # alpha = wgrp
               theme_bw() + labs(x = var[1], y = var[2]) + # guides(alpha = FALSE)
               {if (! plt_grp) scale_fill_gradient('Par. dep.',low = '#99CCFF', high = '#003366')} +
               {if (plt_grp) scale_fill_brewer('avg. PD', palette = 'Blues')})
    }
    # Two factor variables
    if (all(sapply(pd[c('x1', 'x2')], function(x) class(x)[1]) %in% c('factor', 'ordered'))) {
      ordr <- which(sapply(pd[c('x1', 'x2')], function(x) class(x)[1]) == 'ordered')
      ordr <- ifelse(length(ordr) > 0, ordr[1], 1)
      return(pd %>% dplyr::mutate(cross = paste(x1,'*',x2)) %>% ggplot(aes(x = reorder(cross, as.numeric(!!rlang::sym(paste0('x', ordr)))))) +
               {if (! plt_grp) geom_point(aes(y = y, size = w))} +
               {if (plt_grp) geom_point(aes(y = y, size = w, shape = as.factor(xgrp)))} +
               theme_bw() + labs(x = paste(var[1],'*',var[2]), y = 'Partial dependence') + theme(legend.position = 'none', axis.text.x = element_text(angle = 90)))
    }
    # One continuous and one factor variable
    cont <- which(sapply(pd[c('x1', 'x2')], function(x) class(x)[1]) %in% c('integer', 'numeric'))
    fact <- which(sapply(pd[c('x1', 'x2')], function(x) class(x)[1]) %in% c('factor', 'ordered'))
    return(ggplot(pd, aes(x = !!rlang::sym(paste0('x', cont)), colour = !!rlang::sym(paste0('x', fact)), group = !!rlang::sym(paste0('x', fact)))) +
             {if (! plt_grp) geom_line(aes(y = y))} + # alpha = w
             {if (plt_grp) geom_line(aes(y = ygrp))} + # alpha = wgrp
             theme_bw() + labs(x = var[cont], y = ifelse(plt_grp, 'Average partial dependence', 'Partial dependence'), colour = var[fact])) # guides(alpha = FALSE)
  }
}
