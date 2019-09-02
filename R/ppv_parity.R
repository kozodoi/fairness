#' @title Positive Predictive Value parity
#'
#' @description
#' This function computes the Positive Predictive Value (PPV) parity metric
#'
#' @details
#' This function computes the Positive Predictive Value (PPV) parity metric as described by the Aeuquitas bias toolkit.
#' Positive Predictive Values (otherwise known as precision) are calculated
#' by the division of true positives with all predicted positives. In the returned
#' named vector, the reference group will be assigned 1, while all other groups will be assigned values
#' according to whether their positive predictive values are lower or higher compared to the reference group. Lower
#' positive predictive values will be reflected in numbers lower than 1 in the returned named vector, thus numbers
#' lower than 1 mean WORSE prediction for the subgroup.
#'
#' @param data The dataframe that contains the necessary columns.
#' @param outcome The column name of the actual outcomes.
#' @param group Sensitive group to examine.
#' @param probs The column name of the predicted probabilities (numeric between 0 - 1). If not defined, argument preds need to be defined.
#' @param preds The column name of the predicted outcome (categorical outcome). If not defined, argument probs need to be defined.
#' @param preds_levels The desired levels of the predicted outcome (categorical outcome). As these levels are commonly defined as yes/no, the function uses this as default.
#' @param cutoff Cutoff to generate predicted outcomes from predicted probabilities. Default set to 0.5.
#' @param base Base level for sensitive group comparison
#'
#' @name ppv_parity
#'
#' @return
#' Positive Predictive Value parity metrics for all groups. Lower values compared to the reference group mean lower positive predictive values in the selected subgroups.
#'
#' @examples
#' df <- fairness::compas
#' ppv_parity(data = df, outcome = df$score, group = df$race, base = "Caucasian")
#'
#' @export

ppv_parity <- function(data, outcome, group, probs = NULL, preds = NULL,
                       preds_levels = c("no","yes"), cutoff = 0.5, base = NULL) {

  # convert types, sync levels
  group_status <- as.factor(data[,group])
  outcome_status <- as.factor(data[,outcome])
  levels(outcome_status) <- preds_levels
  if (is.null(probs)) {
    preds_status <- as.factor(data[,preds])
  } else {
    preds_status <- as.factor(as.numeric(data[,probs] > cutoff))
  }
  levels(preds_status) <- preds_levels

  # check lengths
  if ((length(outcome_status) != length(preds_status)) |
      (length(outcome_status) != length(group_status))) {
    stop("Outcomes, predictions/probabilities and group status must be of the same length")
  }

  # relevel group
  if (is.null(base)) {
    base <- levels(group_status)[1]
  }
  group_status <- relevel(group_status, base)

  # placeholder
  val <- rep(NA, length(levels(group_status)))
  names(val) <- levels(group_status)

  # compute value for base group
  cm_base <- caret::confusionMatrix(preds_status[group_status == base],
                                    outcome_status[group_status == base], mode = "everything")
  metric_base <- cm_base$byClass[3]

  # compute value for other groups
  for (i in levels(group_status)) {
    cm <- caret::confusionMatrix(preds_status[group_status == i],
                                 outcome_status[group_status == i], mode = "everything")
    metric_i <- cm$byClass[3]
    val[i] <- metric_i / metric_base
  }

  return(val)
}
