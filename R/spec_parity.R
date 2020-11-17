#' @title Specificity parity
#'
#' @description
#' This function computes the Specificity parity metric
#'
#' @details
#' This function computes the Specificity parity metric. Specificities are calculated
#' by the division of true negatives with all negatives (irrespective of predicted values). In the returned
#' named vector, the reference group will be assigned 1, while all other groups will be assigned values
#' according to whether their specificities are lower or higher compared to the reference group. Lower
#' specificities will be reflected in numbers lower than 1 in the returned named vector, thus numbers
#' lower than 1 mean WORSE prediction for the subgroup.
#'
#' @param data The dataframe that contains the necessary columns.
#' @param outcome The column name of the actual outcomes.
#' @param group Sensitive group to examine.
#' @param probs The column name or vector of the predicted probabilities (numeric between 0 - 1). If not defined, argument preds needs to be defined.
#' @param preds The column name or vector of the predicted binary outcome (0 or 1). If not defined, argument probs needs to be defined.
#' @param preds_levels The desired levels of the predicted binary outcome. If not defined, levels of the outcome variable are used.
#' @param outcome_base Base level for the target variable used to compute fairness metrics. Default is the first level of the outcome variable.
#' @param cutoff Cutoff to generate predicted outcomes from predicted probabilities. Default set to 0.5.
#' @param base Base level for sensitive group comparison.
#' @param group_breaks If group is continuous (e.g., age): either a numeric vector of two or more unique cut points or a single number >= 2 giving the number of intervals into which group feature is to be cut.
#'
#' @name spec_parity
#'
#' @return
#' \item{Metric}{Raw specificity metrics for all groups and metrics standardized for the base group (specificity parity metric). Lower values compared to the reference group mean lower specificities in the selected subgroups}
#' \item{Metric_plot}{Bar plot of Specificity parity metric}
#' \item{Probability_plot}{Density plot of predicted probabilities per subgroup. Only plotted if probabilities are defined}
#'
#' @examples
#' data(compas)
#' spec_parity(data = compas, outcome = 'Two_yr_Recidivism', group = 'ethnicity',
#' probs = 'probability', preds = NULL, preds_levels = c('no', 'yes'),
#' cutoff = 0.4, base = 'Caucasian')
#' spec_parity(data = compas, outcome = 'Two_yr_Recidivism', group = 'ethnicity',
#' probs = NULL, preds = 'predicted', preds_levels = c('no', 'yes'),
#' cutoff = 0.5, base = 'Hispanic')
#'
#' @export

spec_parity <- function(data, outcome, group,
                        probs = NULL, 
                        preds = NULL, 
                        preds_levels = NULL, 
                        outcome_base = NULL, 
                        cutoff = 0.5, 
                        base = NULL,
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
    
    group_status   <- as.factor(data[, group])
    outcome_status <- as.factor(data[, outcome])
    
    if (is.null(preds_levels)) {
        preds_levels <- levels(outcome_status)
    }
    levels(preds_status) <- preds_levels
    outcome_status <- relevel(outcome_status, preds_levels[1])
    preds_status   <- relevel(preds_status,   preds_levels[1])

    # check lengths
    if ((length(outcome_status) != length(preds_status)) | (length(outcome_status) !=
        length(group_status))) {
        stop('Outcomes, predictions/probabilities and group status must be of the same length')
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
    
    # set outcome base
    if (is.null(outcome_base)) {
        outcome_base <- levels(preds_status)[1]
    }

    # compute value for all groups
    for (i in levels(group_status)) {
        cm <- caret::confusionMatrix(preds_status[group_status == i], 
                                     outcome_status[group_status == i], 
                                     mode = 'everything',
                                     positive = outcome_base)
        metric_i       <- cm$byClass['Specificity']
        val[i]         <- metric_i
        sample_size[i] <- sum(cm$table)
    }
    
    # aggregate results
    res_table <- rbind(val, val/val[[1]], sample_size)
    rownames(res_table) <- c('Specificity', 'Specificity Parity', 'Group size')

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
        coord_flip() + theme(legend.position = 'none') + labs(x = '', y = 'Specificity Parity')

    # plotting
    if (!is.null(probs)) {
        probs_vals <- data[, probs]
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
