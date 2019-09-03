#' @title ROC AUC parity
#'
#' @description
#' This function computes the Accuracy parity metric
#'
#' @details
#' This function computes the ROC AUC values for each subgroup. In the returned
#' named vector, the reference group will be assigned 1, while all other groups will be assigned values
#' according to whether their ROC AUC values are lower or higher compared to the reference group. Lower
#' ROC AUC will be reflected in numbers lower than 1 in the returned named vector, thus numbers
#' lower than 1 mean WORSE prediction for the subgroup.
#'
#' @param data The dataframe that contains the necessary columns.
#' @param outcome The column name of the actual outcomes.
#' @param group Sensitive group to examine.
#' @param probs The column name of the predicted probabilities (numeric between 0 - 1). If not defined, argument preds need to be defined.
#' @param preds The column name of the predicted outcome (categorical outcome). If not defined, argument probs need to be defined.
#' @param outcome_levels The desired levels of the predicted outcome (categorical outcome). As these levels are commonly defined as yes/no, the function uses this as default.
#' @param cutoff Cutoff to generate predicted outcomes from predicted probabilities. Default set to 0.5.
#' @param base Base level for sensitive group comparison
#'
#' @name roc_parity
#'
#' @return
#' \item{Metric}{ROC AUC values for all groups. Lower values compared to the reference group mean lower ROC AUC values in the selected subgroups}
#' \item{ROCAUC_overlap}{The value of overlapping ROC AUC values}
#' \item{Metric_plot}{Bar plot of ROC AUC metric}
#' \item{Probability_plot}{Density plot of predicted probabilities per subgroup}
#' \item{ROCAUC_plot}{ROC plots for all subgroups}
#'
#' @examples
#' df <- fairness::compas
#' roc_parity(data = df, outcome = df$score, group = df$race, base = "Caucasian")
#'
#' @export

roc_parity <- function(data, outcome, group, probs,
                       outcome_levels = c("no","yes"), base = NULL) {

}
