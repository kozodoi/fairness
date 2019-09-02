#' Predictive Rate Parity
#'
#' This function computes the Predictive Rate Parity
#'
#'
#' @param actuals Vector of actual target values
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return Predictive Rate Parity metric
#' @examples
#' df = fairness::compas
#' pr_parity(df$label_value, df$score, df$race, "Caucasian")
#' @export


pr_parity <- function(actuals, predicted, group, cutoff = 0.5, base = NULL) {


  return(val)
}
