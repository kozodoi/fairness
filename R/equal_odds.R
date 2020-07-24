#' @title Equalized Odds
#'
#' @description
#' This function computes the Equalized Odds metric
#'
#' @details
#' This function computes the Equalized Odds metric (also known as Equal Opportunity, Positive Rate Parity or Separation). Equalized Odds are calculated
#' by the division of true positives with all positives (irrespective of predicted values). This metrics equals to
#' what is traditionally known as sensitivity. In the returned
#' named vector, the reference group will be assigned 1, while all other groups will be assigned values
#' according to whether their sensitivities are lower or higher compared to the reference group. Lower
#' sensitivities will be reflected in numbers lower than 1 in the returned named vector, thus numbers
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
#' @param base Base level for sensitive group comparison
#'
#' @name equal_odds
#'
#' @return
#' \item{Metric}{Raw sensitivities for all groups and metrics standardized for the base group (equalized odds parity metric). Lower values compared to the reference group mean lower sensitivities in the selected subgroups}
#' \item{Metric_plot}{Bar plot of Equalized Odds metric}
#' \item{Probability_plot}{Density plot of predicted probabilities per subgroup. Only plotted if probabilities are defined}
#'
#'
#' @examples
#' data(compas)
#' equal_odds(data = compas, outcome = 'Two_yr_Recidivism', group = 'ethnicity',
#' probs = 'probability', preds = NULL, preds_levels = c('no', 'yes'),
#' cutoff = 0.4, base = 'Caucasian')
#' equal_odds(data = compas, outcome = 'Two_yr_Recidivism', group = 'ethnicity',
#' probs = NULL, preds = 'predicted', preds_levels = c('no', 'yes'),
#' cutoff = 0.5, base = 'Hispanic')
#'
#' @export

equal_odds <- function(data, outcome, group,
                       probs = NULL, preds = NULL, preds_levels = NULL, outcome_base = NULL, 
                       cutoff = 0.5, base = NULL) {
    
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

    # placeholder
    val <- rep(NA, length(levels(group_status)))
    names(val) <- levels(group_status)
    
    # set outcome base
    if (is.null(outcome_base)) {
        outcome_base <- levels(preds_status)[1]
    }

    # compute value for all groups
    for (i in levels(group_status)) {
        cm <- caret::confusionMatrix(preds_status[group_status   == i], 
                                     outcome_status[group_status == i], 
                                     mode = 'everything', 
                                     positive = outcome_base)
        metric_i <- cm$byClass['Sensitivity']
        val[i] <- metric_i
    }
    
    res_table <- rbind(val, val/val[[1]])
    rownames(res_table) <- c('Sensitivity', 'Equalized odds')

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
        coord_flip() + theme(legend.position = 'none') + labs(x = '', y = 'Equalized Odds')

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
