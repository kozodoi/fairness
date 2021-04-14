#' @title Matthews Correlation Coefficient parity
#'
#' @description
#' This function computes the Matthews Correlation Coefficient (MCC) parity metric
#' 
#' Formula: (TP × TN - FP × FN) / sqrt((TP + FP) × (TP + FN) × (TN + FP) × (TN + FN))
#'
#' @details
#' This function computes the Matthews Correlation Coefficient (MCC) parity metric. In the returned
#' named vector, the reference group will be assigned 1, while all other groups will be assigned values
#' according to whether their Matthews Correlation Coefficients are lower or higher compared to the reference group. Lower
#' Matthews Correlation Coefficients rates will be reflected in numbers lower than 1 in the returned named vector, thus numbers
#' lower than 1 mean WORSE prediction for the subgroup.
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
#' @name mcc_parity
#'
#' @return
#' \item{Metric}{Raw Matthews Correlation Coefficient metrics for all groups and metrics standardized for the base group (parity metric). Lower values compared to the reference group mean Matthews Correlation Coefficients in the selected subgroups}
#' \item{Metric_plot}{Bar plot of Matthews Correlation Coefficient metric}
#' \item{Probability_plot}{Density plot of predicted probabilities per subgroup. Only plotted if probabilities are defined}
#'
#' @examples
#' data(compas)
#' compas$Two_yr_Recidivism_01 <- ifelse(compas$Two_yr_Recidivism == 'yes', 1, 0) 
#' mcc_parity(data = compas, outcome = 'Two_yr_Recidivism_01', group = 'ethnicity',
#' probs = 'probability', cutoff = 0.4, base = 'Caucasian')
#' mcc_parity(data = compas, outcome = 'Two_yr_Recidivism_01', group = 'ethnicity',
#' preds = 'predicted', cutoff = 0.5, base = 'Hispanic')
#'
#' @export

mcc_parity <- function(data, outcome, group,
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
        preds_status         <- as.factor(as.numeric(probs > cutoff))
        levels(preds_status) <- levels(as.factor(data[, outcome]))
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
    
    # check levels matching
    if (!identical(levels(outcome_status), levels(preds_status))) {
        warn_preds   <- paste0(levels(preds_status),   collapse = ', ')
        warn_outcome <- paste0(levels(outcome_status), collapse = ', ')
        stop({paste0(c('Levels of predictions and outcome do not match. ',
                       'Please relevel predictions or outcome.\n',
                       'Outcome levels: ', warn_preds, '\n',
                       'Preds   levels: ', warn_outcome))})}
    
    # relevel preds & outcomes
    if (is.null(outcome_base)) {
        outcome_base <- levels(outcome_status)[1]
    }else{
        outcome_base <- as.character(outcome_base)
    }
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
        TP <- as.numeric(cm$table[cm_positive, cm_positive])
        TN <- as.numeric(cm$table[cm_negative, cm_negative])
        FP <- as.numeric(cm$table[cm_positive, cm_negative])
        FN <- as.numeric(cm$table[cm_negative, cm_positive])
        numerator <- (TP * TN) - (FP * FN)
        denominator <- sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
        if (denominator == 0) {
            metric_i <- 0
        } else {
            metric_i <- numerator / denominator
        }
        val[i] <- metric_i
        sample_size[i] <- sum(cm$table)
    }
    
    # aggregate results
    res_table <- rbind(val, val/val[[1]], sample_size)
    rownames(res_table) <- c('MCC', 'MCC Parity', 'Group size')

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
        coord_flip() + theme(legend.position = 'none') + labs(x = '', y = 'Matthews Correlation Coefficient Parity')

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
