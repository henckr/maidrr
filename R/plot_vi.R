#' Plot the variable importance
#'
#' Plot variable importance scores for the predictors in a model.
#'
#' @param vi A data frame containing variable importance scores as returned by
#'   \code{\link{get_vi}}.
#' @return A ggplot object illustrating the variable importance scores.
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
#' gbm_fit %>% get_vi %>% plot_vi
#' gbm_fit %>% get_vi(scale = TRUE) %>% plot_vi
#' gbm_fit %>% get_vi %>% dplyr::mutate(Importance = Importance / 100) %>% plot_vi + scale_y_continuous(labels = scales::percent)
#'
#' }
#' @export
plot_vi <- function(vi) {
  vi %>% ggplot(aes(x = reorder(Variable,Importance), Importance)) +
    geom_bar(stat = 'identity') + theme_bw() + labs(x = '', y = 'Importance') + coord_flip()
}
