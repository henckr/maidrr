#' Predefined error functions
#'
#' Some predefined error functions to use in \code{\link{autotune}}.
#'
#' @param y_true vector of the true target values.
#' @param y_pred vector of the predicted target values.
#' @param w_case vector of case weights.
#' @keywords internal
err_fun <- function(y_true, y_pred, w_case) {}

#' @describeIn err_fun Poisson deviance loss function
#' @export
poi_dev <- function(y_true, y_pred) -2 * sum(dpois(y_true, y_pred, log = TRUE) - dpois(y_true, y_true, log = TRUE), na.rm = TRUE) / length(y_pred[!is.na(y_pred)])

#' @describeIn err_fun Mean squared error loss function
#' @export
mse <- function(y_true, y_pred) sum((y_true - y_pred)^2, na.rm = TRUE) / length(y_pred[!is.na(y_pred)])

#' @describeIn err_fun Weighted mean squared error loss function
#' @export
wgt_mse <- function(y_true, y_pred, w_case) sum(w_case * (y_true - y_pred)^2, na.rm = TRUE) / length(y_pred[!is.na(y_pred)])
