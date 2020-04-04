#' maidrr: Model-Agnostic Interpretable Data-driven suRRogate
#'
#' The goal of maidrr is to aid you in the development of a Model-Agnostic Interpretable Data-driven suRRogate for your black box algorithm of choice.
#' In short, these are the steps in the procedure:
#' 1) Partial dependencies (PDs) are used to obtain model insights from the black box in the form of feature effects.
#' 2) Those effects are used to group values/levels within a feature in an optimal data-driven way, while performing built-in feature selection.
#' 3) An interpretable GLM surrogate is fit to the segmented features. Meaningful interactions can be included if desired.
#'
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar%
#'
#' @docType package
#' @name maidrr-package
#' @keywords package
NULL
