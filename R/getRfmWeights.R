#' Get RFM weights
#'
#' @param df data frame or tibble.
#' @param formula formula for the Generalized Linear Model (GLM).
#' @param family family for the GLM; optional, default value is \code{binomial}.
#' @param show show regression table and weights; optional, default value is \code{TRUE}.
#' @return A list containing an atomic vector of weights and a GLM model.
#' @export

getRfmWeights <- function (df, formula,
                           family = "binomial", show = TRUE) {

  warning("x variables must be standardized before using this function")

  model <- glm(formula,
               data = df,
               family = family)

  coeffs <- coefficients(model)
  coeffs <- coeffs[-match("(Intercept)", names(coeffs))]

  if (length(coeffs) > 3) {
    warning("rfm only needs three weights!")
  }

  if (family == "binomial") {
    oddsRatios <- exp(coeffs)
    weights <- oddsRatios / sum(oddsRatios, na.rm = TRUE)
  } else {
    weights <- coeffs / sum(coeffs, na.rm = TRUE)
  }

  names(weights) <- paste0(names(coeffs), "_weight")

  if (show) {
    stargazer::stargazer(model, type = "text")
    print(weights)
  }

  return(list(weights, model))

}
