#' @title False Positive Rate parity
#'
#' @description
#' This function computes the False Positive Rate (FPR) parity metric
#' 
#' Formula: FP / (TN + FP)
#'
#' @details
#' This function computes the False Positive Rate (FPR) parity metric as described by Chouldechova 2017. False positive rates are calculated
#' by the division of false positives with all negatives (irrespective of predicted values). In the returned
#' named vector, the reference group will be assigned 1, while all other groups will be assigned values
#' according to whether their false positive rates are lower or higher compared to the reference group. Lower
#' false positives error rates will be reflected in numbers lower than 1 in the returned named vector, thus numbers
#' lower than 1 mean BETTER prediction for the subgroup.
#'
#' @param data Data.frame that contains the necessary columns.
#' @param group Column name indicating the sensitive group (character).
#' @param base Base level of the sensitive group (character).
#' @param group_breaks If group is continuous (e.g., age): either a numeric vector of two or more unique cut points or a single number >= 2 giving the number of intervals into which group feature is to be cut.
#' @param outcome Column name indicating the binary outcome variable (character).
#' @param outcome_base Base level of the outcome variable (i.e., negative class). Default is the first level of the outcome variable.
#' @param probs Column name or vector with the predicted probabilities (numeric between 0 - 1). Either probs or preds need to be supplied.
#' @param preds Column name or vector with the predicted binary outcome (0 or 1). Either probs or preds need to be supplied.
#' @param cutoff Cutoff to generate predicted outcomes from predicted probabilities. Default set to 0.5.
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
#' compas$Two_yr_Recidivism_01 <- ifelse(compas$Two_yr_Recidivism == 'yes', 1, 0) 
#' fpr_parity(data = compas, outcome = 'Two_yr_Recidivism_01', group = 'ethnicity',
#' probs = 'probability', cutoff = 0.4, base = 'Caucasian')
#' fpr_parity(data = compas, outcome = 'Two_yr_Recidivism_01', group = 'ethnicity',
#' preds = 'predicted', cutoff = 0.5, base = 'Hispanic')
#'
#' @export

fpr_parity <- function(data, outcome, group,
                       probs        = NULL, 
                       preds        = NULL, 
                       outcome_base = NULL, 
                       cutoff       = 0.5, 
                       base         = NULL,
                       group_breaks = NULL) {
    
    # check if data is data.frame
    if (class(data)[1] != 'data.frame') {
        warning(paste0('Converting ', class(data)[1], ' to data.frame'))
        data <- as.data.frame(data)
    }

    # convert types, sync levels
    if (is.null(probs) & is.null(preds)) {
        stop({'Either probs or preds have to be supplied'})
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
    
    # check group feature and cut if needed
    if ((length(unique(data[, group])) > 10) & (is.null(group_breaks))) {
        warning('Number of unqiue group levels exceeds 10. Consider specifying `group_breaks`.')
    }
    if (!is.null(group_breaks)) {
        if (is.numeric(data[, group])) {
            data[, group] <- cut(data[, group], breaks = group_breaks)
        }else{
            warning('Attempting to bin a non-numeric group feature.')
        }
    }
    
    # convert to factor
    group_status   <- as.factor(data[, group])
    outcome_status <- as.factor(data[, outcome])
    
    # relevel preds & outcomes
    if (is.null(outcome_base)) {outcome_base <- levels(outcome_status)[1]}
    outcome_status   <- relevel(outcome_status, outcome_base)
    preds_status     <- relevel(preds_status,   outcome_base)
    outcome_positive <- levels(outcome_status)[2]

    # check lengths
    if ((length(outcome_status) != length(preds_status)) | (length(outcome_status) !=
        length(group_status))) {
        stop('Outcomes, predictions/probabilities and group status must be of the same length')
    }

    # relevel group
    if (is.null(base)) {base <- levels(group_status)[1]}
    group_status <- relevel(group_status, base)

    # placeholders
    val         <- rep(NA, length(levels(group_status)))
    names(val)  <- levels(group_status)
    sample_size <- val

    # compute value for all groups
    for (i in levels(group_status)) {
        cm <- caret::confusionMatrix(preds_status[group_status   == i], 
                                     outcome_status[group_status == i], 
                                     mode     = 'everything',
                                     positive = outcome_positive)
        cm_positive <- cm$positive
        cm_negative <- levels(outcome_status)[!(levels(outcome_status) %in% cm_positive)]
        TP <- cm$table[cm_positive, cm_positive]
        TN <- cm$table[cm_negative, cm_negative]
        FP <- cm$table[cm_positive, cm_negative]
        FN <- cm$table[cm_negative, cm_positive]
        metric_i <- FP / (FP + TN)
        val[i] <- metric_i
        sample_size[i] <- sum(cm$table)
    }

    # aggregate results
    res_table <- rbind(val, val/val[[1]], sample_size)
    rownames(res_table) <- c('FPR', 'FPR Parity', 'Group size')

    # conversion of metrics to df
    val_df <- as.data.frame(res_table[2, ])
    colnames(val_df) <- c('val')
    val_df$groupst <- rownames(val_df)
    val_df$groupst <- as.factor(val_df$groupst)

    # relevel group
    if (is.null(base)) {
        val_df$groupst <- levels(val_df$groupst)[1]
    }
    val_df$groupst <- relevel(val_df$groupst, base)

    p <- ggplot(val_df, aes(x = groupst, weight = val, fill = groupst)) + geom_bar(alpha = 0.5) +
        coord_flip() + theme(legend.position = 'none') + labs(x = '', y = 'False Positive Rate Parity')

    # plotting
    if (!is.null(probs)) {
        q <- ggplot(data, aes(x = probs, fill = group_status)) + geom_density(alpha = 0.5) +
            labs(x = 'Predicted probabilities') + guides(fill = guide_legend(title = '')) +
            theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 1) + geom_vline(xintercept = cutoff,
            linetype = 'dashed')
    }

    if (is.null(probs)) {
        list(Metric = res_table, Metric_plot = p)
    } else {
        list(Metric = res_table, Metric_plot = p, Probability_plot = q)
    }

}
