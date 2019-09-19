#' @title False Positive Rate parity
#'
#' @description
#' This function computes the False Positive Rate (FPR) parity metric
#'
#' @details
#' This function computes the False Positive Rate (FPR) parity metric as described by Chouldechova 2017. False positive rates are calculated
#' by the division of false positives with all negatives (irrespective of predicted values). In the returned
#' named vector, the reference group will be assigned 1, while all other groups will be assigned values
#' according to whether their false positive rates are lower or higher compared to the reference group. Lower
#' false positives error rates will be reflected in numbers lower than 1 in the returned named vector, thus numbers
#' lower than 1 mean BETTER prediction for the subgroup.
#'
#' @param data The dataframe that contains the necessary columns.
#' @param outcome The column name of the actual outcomes.
#' @param group Sensitive group to examine.
#' @param probs The column name or vector of the predicted probabilities (numeric between 0 - 1). If not defined, argument preds needs to be defined.
#' @param preds The column name or vector of the predicted outcome (categorical outcome). If not defined, argument probs needs to be defined.
#' @param outcome_levels The desired levels of the predicted outcome (categorical outcome). If not defined, all unique values of outcome are used.
#' @param cutoff Cutoff to generate predicted outcomes from predicted probabilities. Default set to 0.5.
#' @param base Base level for sensitive group comparison
#'
#' @name fpr_parity
#'
#' @return
#' \item{Metric}{Raw false positive rates for all groups and metrics standardized for the base group (false positive rate parity metric). Lower values compared to the reference group mean lower false positive error rates in the selected subgroups}
#' \item{Metric_plot}{Bar plot of False Positives Rate metric}
#' \item{Probability_plot}{Density plot of predicted probabilities per subgroup. Only plotted if probabilities are defined}
#'
#' @examples
#' data(compas)
#' fpr_parity(data = compas, outcome = 'Two_yr_Recidivism', group = 'ethnicity',
#' probs = 'probability', preds = NULL, outcome_levels = c('no', 'yes'),
#' cutoff = 0.4, base = 'Caucasian')
#' fpr_parity(data = compas, outcome = 'Two_yr_Recidivism', group = 'ethnicity',
#' probs = NULL, preds = 'predicted', outcome_levels = c('no', 'yes'),
#' cutoff = 0.5, base = 'Hispanic')
#'
#' @export

fpr_parity <- function(data, outcome, group,
                       probs = NULL, preds = NULL, outcome_levels = NULL, cutoff = 0.5, base = NULL) {

    # convert types, sync levels
    group_status <- as.factor(data[, group])
    outcome_status <- as.factor(data[, outcome])
    if (is.null(outcome_levels)) {
        outcome_levels <- unique(outcome_status)
    }
    levels(outcome_status) <- outcome_levels
    if (is.null(probs) & is.null(preds)) {
        stop({"Either probs or preds have to be supplied"})
    }
    if (is.null(probs)) {
        if (length(preds) == 1) {
            preds <- data[, preds]
        }
        preds_status <- as.factor(preds)
    } else {
        if (length(probs) == 1) {
            probs <- data[, probs]
        }
        preds_status <- as.factor(as.numeric(probs > cutoff))
    }
    levels(preds_status) <- outcome_levels

    # check lengths
    if ((length(outcome_status) != length(preds_status)) | (length(outcome_status) !=
        length(group_status))) {
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

    # compute value for all groups
    for (i in levels(group_status)) {
        cm <- caret::confusionMatrix(preds_status[group_status == i], outcome_status[group_status ==
            i], mode = "everything")
        metric_i <- cm$table[2]/sum(cm$table[1], cm$table[2])
        val[i] <- metric_i
    }

    res_table <- rbind(val, val/val[[1]])
    rownames(res_table) <- c("FPR", "FPR Parity")

    # conversion of metrics to df
    val_df <- as.data.frame(res_table[2, ])
    colnames(val_df) <- c("val")
    val_df$groupst <- rownames(val_df)
    val_df$groupst <- as.factor(val_df$groupst)

    # relevel group
    if (is.null(base)) {
        val_df$groupst <- levels(val_df$groupst)[1]
    }
    val_df$groupst <- relevel(val_df$groupst, base)

    p <- ggplot(val_df, aes(x = groupst, weight = val, fill = groupst)) + geom_bar(alpha = 0.5) +
        coord_flip() + theme(legend.position = "none") + labs(x = "", y = "False Positive Rate Parity")

    # plotting
    if (!is.null(probs)) {
        q <- ggplot(data, aes(x = probs, fill = group_status)) + geom_density(alpha = 0.5) +
            labs(x = "Predicted probabilities") + guides(fill = guide_legend(title = "")) +
            theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 1) + geom_vline(xintercept = cutoff,
            linetype = "dashed")
    }

    if (is.null(probs)) {
        list(Metric = res_table, Metric_plot = p)
    } else {
        list(Metric = res_table, Metric_plot = p, Probability_plot = q)
    }

}
