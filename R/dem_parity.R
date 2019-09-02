#' Demographic Parity
#'
#' This function computes the Demographic Parity metric (Calders and Verwer 2010)
#'
#'
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return DP metric
#' @export
#' @examples
#' df = fairness::compas
#' dem_parity(df$score, df$race, "Caucasian")

dem_parity <- function(predicted, group, cutoff = 0.5, base = NULL) {

  # check lengths
  if (length(predicted) != length(group)) {
    stop("Predictions and groups must be of the same length")
  }

  # convert types
  group     <- as.factor(group)
  predicted <- as.numeric(predicted >= cutoff)

  # relevel group
  if (is.null(base)) {base <- levels(group)[1]}
  group <- relevel(group, base)

  # placeholder
  val <- rep(NA, length(levels(group)))
  names(val) <- levels(group)

  # compute value for gorup 1
  cv1 <- mean(predicted[group == levels(group)[1]])

  # compute value for other groups
  for (i in 1:length(levels(group))) {
    val[i] <- 1 - (mean(predicted[group == levels(group)[i]]) - cv1)
  }

  return(val)
}
