# Create the hexSticker for maidrr
hexSticker::sticker('maidrr.png', package = 'maidrr',
                    s_x = 1, s_width = 1,
                    p_y = 1.55, p_color = '#2b60de',
                    h_size = 1, h_color = '#2b60de', h_fill = '#e6e6fa',
                    dpi = 720,
                    filename = 'man/figures/maidrr_hex.png')

#' Drop unused factor levels
#'
#' Function to drop empty factor levels in test data
#' (\href{https://stackoverflow.com/a/39495480/4185785}{source}).
#'
#' @param fit Model fit of class "glm".
#' @param test_data Data frame containing the test data.
#' @export
rm_lvls <- function(fit, test_data) {

  test_data %>%
    droplevels() %>%
    as.data.frame() -> test_data

  # Obtain factor predictors in the model and their levels
  factors <- (gsub("[-^0-9]|as.factor|\\(|\\)", "",
                   names(unlist(fit$xlevels))))
  # Do nothing if no factors are present
  if (length(factors) == 0) {
    return(test_data)
  }
  factor_levels <- unname(unlist(fit$xlevels))
  model_factors <- as.data.frame(cbind(factors, factor_levels))

  # Select columns in test data that are factor predictors in trained model
  predictors <- names(test_data[names(test_data) %in% factors])
  # For each factor predictor in your data, set the unused level to NA
  for (i in seq_len(length(predictors))) {
    found <- test_data[, predictors[i]] %in% model_factors[
      model_factors$factors == predictors[i], ]$factor_levels
    if (any(!found)) {
      # track which variable
      var <- predictors[i]
      # set to NA
      test_data[!found, predictors[i]] <- NA
      # drop empty factor levels in test data
      test_data %>%
        droplevels() -> test_data
    }
  }
  return(test_data)
}
