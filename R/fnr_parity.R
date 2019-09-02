#' FNR Parity
#'
#' This function computes the False Negative Rate Parity metric (Chouldechova 2017)
#'
#'
#' @param actuals Vector of actual target values
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return FNR Parity metric
#' @examples
#' df = fairness::compas
#' fnr_parity(df$label_value, df$score, df$race, "Caucasian")
#' @export
fnr_parity <- function(actuals, predicted, group, cutoff = 0.5, base = NULL) {

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

  # compute value for gorup 1
  fnr1 <- sum(actuals == 1 & predicted == 0 & group == levels(group)[1]) /
    sum(actuals == 1 & group == levels(group)[1])

  # compute value for other groups
  for (i in 1:length(levels(group))) {
    fnri <- sum(actuals == 1 & predicted == 0 & group == levels(group)[i]) /
      sum(actuals == 1 & group == levels(group)[i])
    val[i] <- ifelse(fnr1 != 0, fnri / fnr1, 1)
  }

  return(val)
}
