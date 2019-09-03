#' @title Equalized Odds
#'
#' @description
#' This function computes the Equalized Odds metric
#'
#' @details
#' This function computes the Equalized Odds metric (also known as Equal Opportunity, Positive Rate Parity or Separation) as
#' described by Hardt et al., 2016 and Zafar et al., 2017. Equalized Odds is calculated
#' based on ....... In the returned
#' named vector, the reference group will be assigned 1, while all other groups will be assigned values
#' according to whether their proportion of positively predicted observations are lower or higher compared to the reference group. Lower
#' proportions will be reflected in numbers lower than 1 in the returned named vector, thus numbers
#' lower than 1 indicate negative disparity for the given subgroup.
#'
#' @param data The dataframe that contains the necessary columns.
#' @param group Sensitive group to examine.
#' @param probs The column name of the predicted probabilities (numeric between 0 - 1). If not defined, argument preds need to be defined.
#' @param preds The column name of the predicted outcome (categorical outcome). If not defined, argument probs need to be defined.
#' @param preds_levels The desired levels of the predicted outcome (categorical outcome). As these levels are commonly defined as yes/no, the function uses this as default.
#' @param cutoff Cutoff to generate predicted outcomes from predicted probabilities. Default set to 0.5.
#' @param base Base level for sensitive group comparison
#'
#' @name equal_odds
#'
#' @return
#' Demographic parity metrics for all groups. Lower values compared to the reference group mean lower proportion of positively predicted observations in the selected subgroups.
#'
#' @examples
#' df <- fairness::compas
#' equal_odds(data = df, group = df$race, base = "Caucasian")
#'
#' @export


equal_odds <- function(data, group, probs = NULL, preds = NULL,
                       preds_levels = c("no","yes"), cutoff = 0.5, base = NULL) {

  # convert types, sync levels
  group_status <- as.factor(data[,group])
  if (is.null(probs)) {
    levels(data[,preds]) <- c(0,1)
    preds_status <- as.numeric(as.character(data[,preds]))
  } else {
    preds_status <- as.numeric(data[,probs] > cutoff)
  }

  # check lengths
  if (length(group_status) != length(preds_status)) {
    stop("Predictions/probabilities and group status must be of the same length")
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
  metric_base <- mean(preds_status[group_status == base])

  # compute value for other groups
  for (i in levels(group_status)) {
    metric_i <- mean(preds_status[group_status == i])
    val[i] <- metric_i / metric_base
  }

  return(val)
}

