#' Accuracy Parity
#'
#' This function computes the Accuracy Parity metric (Friedler et al. 2018)
#'
#'
#' @param actuals Vector of actual target values
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return Accuracy Parity metric
#' @examples
#' df = fairness::compas
#' acc_parity(df$label_value, df$score, df$race, "Caucasian")
#' @export

acc_parity <- function(actuals, predicted, group, cutoff = 0.5, base = NULL) {

  # check lengths
  if ((length(actuals) != length(predicted)) | (length(actuals) != length(group))) {
    stop("Actuals, predictions and groups must be of the same length")
  }

  # convert types
  group     <- as.factor(group)
  actuals   <- as.numeric(actuals)
  predicted <- as.numeric(predicted >= cutoff)

  # relevel group
  if (is.null(base)) {base <- levels(group)[1]}
  group <- relevel(group, base)

  # placeholder
  val <- rep(NA, length(levels(group)))
  names(val) <- levels(group)

  # compute value for group 1
  ac1 <- sum(actuals[group == levels(group)[1]] == predicted[group == levels(group)[1]]) /
    sum(group == levels(group)[1])

  # compute value for other groups
  for (i in 1:length(levels(group))) {
    aci <- sum(actuals[group == levels(group)[i]] == predicted[group == levels(group)[i]]) /
      sum(group == levels(group)[i])
    val[i] <- ifelse(ac1 != 0, aci / ac1, 1)
  }

  return(val)
}

