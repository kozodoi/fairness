#' Disparate Impact
#'
#' This function computes the Disparate Impact metric (Feldman et al. 2015; Zafar et al. 2017)
#'
#'
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return DI metric
#' @export
#' @examples
#' df = fairness::compas
#' dis_impact(df$label_value, df$score, df$race, "Caucasian")

disp_impact <- function(predicted, group, base = NULL) {

  # check lengths
  if (length(predicted) != length(group)) {
    stop("Predictions and groups must be of the same length")
  }

  # convert types
  group <- as.factor(group)

  # relevel group
  if (is.null(base)) {base <- levels(group)[1]}
  group <- relevel(group, base)

  # placeholder
  val <- rep(NA, length(levels(group)))
  names(val) <- levels(group)

  # compute value for group 1
  cv1 <- mean(predicted[group == levels(group)[1]])

  # compute value for other groups
  for (i in 1:length(levels(group))) {
    val[i] <- mean(predicted[group == levels(group)[i]]) / cv1
  }

  return(val)
}
