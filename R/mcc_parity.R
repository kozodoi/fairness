#' @title Matthews Correlation Coefficient parity
#'
#' @description
#' This function computes the Matthews Correlation Coefficient (MCC) parity metric
#'
#' @details
#' This function computes the Matthews Correlation Coefficient (MCC) parity metric. In the returned
#' named vector, the reference group will be assigned 1, while all other groups will be assigned values
#' according to whether their Matthews Correlation Coefficients are lower or higher compared to the reference group. Lower
#' Matthews Correlation Coefficients rates will be reflected in numbers lower than 1 in the returned named vector, thus numbers
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
#' @name mcc_parity
#'
#' @return
#' Matthews Correlation Coefficient parity metrics for all groups. Lower values compared to the reference group mean Matthews Correlation Coefficients in the selected subgroups.
#'
#' @examples
#' df <- fairness::compas
#' mcc_parity(data = df, outcome = df$score, group = df$race, base = "Caucasian")
#'
#' @export

mcc_parity <- function(data, outcome, group, probs = NULL, preds = NULL,
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
  TP <- as.numeric(cm_base$table[4])
  TN <- as.numeric(cm_base$table[1])
  FP <- as.numeric(cm_base$table[2])
  FN <- as.numeric(cm_base$table[3])
  numerator <- (TP*TN)-(FP*FN)
  denominator <- sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  if (denominator == 0) {
    metric_base <- 0
  } else {
    metric_base <- numerator/denominator
  }

  # compute value for other groups
  for (i in levels(group_status)) {
    cm <- caret::confusionMatrix(preds_status[group_status == i],
                                 outcome_status[group_status == i], mode = "everything")
    TP <- as.numeric(cm$table[4])
    TN <- as.numeric(cm$table[1])
    FP <- as.numeric(cm$table[2])
    FN <- as.numeric(cm$table[3])
    numerator <- (TP*TN)-(FP*FN)
    denominator <- sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    if (denominator == 0) {
      metric_i <- 0
    } else {
      metric_i <- numerator/denominator
    }
    val[i] <- metric_i / mmetric_base
  }

  return(val)
}
