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
#' @param probs The column name of the predicted probabilities (numeric between 0 - 1).
#' @param outcome_levels The desired levels of the predicted outcome (categorical outcome). As these levels are commonly defined as yes/no, the function uses this as default.
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

  # convert types, sync levels
  group_status <- as.factor(data[,group])
  outcome_status <- as.factor(data[,outcome])
  levels(outcome_status) <- outcome_levels
  probs_vals <- as.numeric(data[,probs])

  # check lengths
  if ((length(outcome_status) != length(probs_vals)) |
      (length(outcome_status) != length(group_status))) {
    stop("Outcomes, probabilities and group status must be of the same length")
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
  roc_base <- roc(predictor=probs_vals[group_status == base],
                  response=outcome_status[group_status == base],
                  levels=levels(outcome_status),
                  ci=T)
  val[1] <- as.numeric(roc_base$auc)

  # compute value for other group
  roc_i <- roc(predictor=probs_vals[group_status != base],
               response=outcome_status[group_status != base],
               levels=levels(outcome_status),
               ci=T)
  val[2] <- as.numeric(roc_i$auc)
  val <- val/val[1]

  #find the overlapping ROC AUCs (roc_ol)
  #roc_ol <- roc.test(roc_base, roc_i, method = "bootstrap", boot.n=1000)
  #roc_ol$estimate

  #conversion of metrics to df
  val_df <- as.data.frame(val)
  val_df$groupst <- rownames(val_df)
  val_df$groupst <- as.factor(val_df$groupst)
  # relevel group
  if (is.null(base)) {
    val_df$groupst <- levels(val_df$groupst)[1]
  }
  val_df$groupst <- relevel(val_df$groupst, base)

  p <- ggplot(val_df, aes(x=groupst, weight=val, fill=groupst)) +
    geom_bar(alpha=.5) +
    coord_flip() +
    theme(legend.position = "none") +
    labs(x = "", y = "Predictive Rate Parity")

  probs_vals <- data[,probs]
  q <- ggplot(data, aes(x=probs_vals, fill=group_status)) +
    geom_density(alpha=.5) +
    labs(x = "Predicted probabilities") +
    guides(fill = guide_legend(title = "")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(0,1)

  r <- #plot for ROC curves here

    list(Metric = val, ROCAUC_overlap = roc_ol, Metric_plot = p, Probability_plot = q, ROCAUC_plot = r)

}
