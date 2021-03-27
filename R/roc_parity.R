#' @title ROC AUC parity
#'
#' @description
#' This function computes the ROC AUC parity metric
#'
#' @details
#' This function computes the ROC AUC values for each subgroup. In the returned table,
#' the reference group will be assigned 1, while all other groups will be assigned values
#' according to whether their ROC AUC values are lower or higher compared to the reference group. Lower
#' ROC AUC will be reflected in numbers lower than 1 in the returned named vector, thus numbers
#' lower than 1 mean WORSE prediction for the subgroup.
#'
#' @param data Data.frame that contains the necessary columns.
#' @param group Column name indicating the sensitive group (character).
#' @param base Base level of the sensitive group (character).
#' @param group_breaks If group is continuous (e.g., age): either a numeric vector of two or more unique cut points or a single number >= 2 giving the number of intervals into which group feature is to be cut.
#' @param outcome Column name indicating the binary outcome variable (character).
#' @param probs Column name or vector with the predicted probabilities (numeric between 0 - 1). 
#' 
#' @name roc_parity
#'
#' @return
#' \item{Metric}{Raw ROC AUC metrics for all groups and metrics standardized for the base group (parity metric). Lower values compared to the reference group mean lower ROC AUC values in the selected subgroups}
#' \item{Metric_plot}{Bar plot of ROC AUC metric}
#' \item{Probability_plot}{Density plot of predicted probabilities per subgroup}
#' \item{ROCAUC_plot}{ROC plots for all subgroups}
#'
#' @examples
#' data(compas)
#' compas$Two_yr_Recidivism_01 <- ifelse(compas$Two_yr_Recidivism == 'yes', 1, 0) 
#' roc_parity(data = compas, outcome = 'Two_yr_Recidivism_01', group = 'ethnicity',
#' probs = 'probability', base = 'Caucasian')
#' roc_parity(data = compas, outcome = 'Two_yr_Recidivism_01', group = 'ethnicity',
#' probs = 'probability', base = 'African_American')
#'
#' @export


roc_parity <- function(data, outcome, group, probs,
                       base = NULL,
                       group_breaks = NULL) {
    
    # check if data is data.frame
    if (class(data)[1] != 'data.frame') {
        warning(paste0('Converting ', class(data)[1], ' to data.frame'))
        data <- as.data.frame(data)
    }

    # convert types, sync levels
    if (is.null(probs)) {
        stop({'Probs have to be supplied'})
    }

    if (length(probs) == 1) {
        probs <- data[, probs]
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

    group_status   <- as.factor(data[, group])
    outcome_status <- as.factor(data[, outcome])
    
    # check lengths
    if ((length(outcome_status) != length(probs)) | (length(outcome_status) !=
        length(group_status))) {
        stop('Outcomes, probabilities and group status must be of the same length')
    }

    # relevel group
    if (is.null(base)) {
        base <- levels(group_status)[1]
    }
    group_status <- relevel(group_status, base)

    # placeholders
    val         <- rep(NA, length(levels(group_status)))
    names(val)  <- levels(group_status)
    sample_size <- val
    
    # compute value for all groups
    for (i in 1:length(levels(group_status))) {
        temproc <- pROC::roc(predictor = probs[group_status == levels(group_status)[i]],
            response = outcome_status[group_status == levels(group_status)[i]], 
            levels = levels(outcome_status), ci = T, quiet = T)
        val[i] <- as.numeric(temproc[[9]])
        assign(paste0('grouproc_', i), temproc)
        sample_size[i] <- length(probs[group_status == levels(group_status)[i]])
    }

    if (length(levels(group_status)) == 2) {
        r <- pROC::ggroc(list(grouproc_1, grouproc_2)) + geom_abline(intercept = 1,
            slope = 1) + labs(x = 'Specificity', y = 'Sensitivity') + theme(legend.title = element_blank()) +
            scale_color_discrete(labels = names(val))
    } else if (length(levels(group_status)) == 3) {
        r <- pROC::ggroc(list(grouproc_1, grouproc_2, grouproc_3)) + geom_abline(intercept = 1,
            slope = 1) + labs(x = 'Specificity', y = 'Sensitivity') + theme(legend.title = element_blank()) +
            scale_color_discrete(labels = names(val))
    } else if (length(levels(group_status)) == 4) {
        r <- pROC::ggroc(list(grouproc_1, grouproc_2, grouproc_3, grouproc_4)) + geom_abline(intercept = 1,
            slope = 1) + labs(x = 'Specificity', y = 'Sensitivity') + theme(legend.title = element_blank()) +
            scale_color_discrete(labels = names(val))
    } else if (length(levels(group_status)) == 5) {
        r <- pROC::ggroc(list(grouproc_1, grouproc_2, grouproc_3, grouproc_4, grouproc_5)) +
            geom_abline(intercept = 1, slope = 1) + labs(x = 'Specificity', y = 'Sensitivity') +
            theme(legend.title = element_blank()) + scale_color_discrete(labels = names(val))
    } else if (length(levels(group_status)) == 6) {
        r <- pROC::ggroc(list(grouproc_1, grouproc_2, grouproc_3, grouproc_4, grouproc_5,
            grouproc_6)) + geom_abline(intercept = 1, slope = 1) + labs(x = 'Specificity',
            y = 'Sensitivity') + theme(legend.title = element_blank()) + scale_color_discrete(labels = names(val))
    } else {
        r <- NULL
    }
    
    # aggregate results
    res_table <- rbind(val, val/val[[1]], sample_size)
    rownames(res_table) <- c('ROC AUC', 'ROC AUC Parity', 'Group size')

    # conversion of metrics to df
    val_df <- as.data.frame(res_table[2, ])
    colnames(val_df) <- c('val')
    val_df$groupst   <- rownames(val_df)
    val_df$groupst   <- as.factor(val_df$groupst)
    
    # relevel group
    if (is.null(base)) {
        val_df$groupst <- levels(val_df$groupst)[1]
    }
    val_df$groupst <- relevel(val_df$groupst, base)

    p <- ggplot(val_df, aes(x = groupst, weight = val, fill = groupst)) + geom_bar(alpha = 0.5) +
        coord_flip() + theme(legend.position = 'none') + labs(x = '', y = 'Predictive Rate Parity')

    q <- ggplot(data, aes(x = probs, fill = group_status)) + geom_density(alpha = 0.5) +
        labs(x = 'Predicted probabilities') + guides(fill = guide_legend(title = '')) +
        theme(plot.title = element_text(hjust = 0.5)) + xlim(0, 1)

    list(Metric = res_table, Metric_plot = p, Probability_plot = q, ROCAUC_plot = r)

}
