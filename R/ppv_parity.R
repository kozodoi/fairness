#' PPV Parity
#'
#' This function computes the Positive Predicted Value Parity metric (see Aeuquitas bias audit toolkit)
#'
#'
#' @param actuals Vector of actual target values
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return FNR Parity metric
#' @examples
#' df = fairness::compas
#' ppv_pariy(df$label_value, df$score, df$race, "Caucasian")
#' @export
ppv_parity <- function(actuals, predicted, group, cutoff = 0.5, base = NULL) {

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
  ppv1 <- sum(actuals == 1 & predicted == 1 & group == levels(group)[1]) /
    sum(predicted == 1 & group == levels(group)[1])

  # compute value for other groups
  for (i in 1:length(levels(group))) {
    ppvi <- sum(actuals == 1 & predicted == 1 & group == levels(group)[i]) /
      sum(predicted == 1 & group == levels(group)[i])
    val[i] <- ifelse(ppv1 != 0, ppvi / ppv1, 1)
  }

  return(val)
}

