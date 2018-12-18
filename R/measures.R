#' Disparate Impact
#'
#' This function computes the Disparate Impact metric (Feldman et al. 2015; Zafar et al. 2017)
#' 
#' 
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return DI metric
#' @export
#' @examples
#' df = fairness::compas
#' dis_impact(df$label_value, df$score, df$race, "Caucasian")
dis_impact <- function(predicted, group, cutoff = 0.5, base = NULL) {
  
  # check lengths
  if (length(predicted) != length(group)) {
    stop("Predictions and groups must be of the same length")
  }
  
  # convert types
  group     <- as.factor(group) 
  predicted <- as.numeric(predicted >= cutoff)
  
  # relevel group
  if (is.null(base)) {base <- levels(group)[1]}
  group <- relevel(group, base)
  
  # placeholder
  val <- rep(NA, length(levels(group)))
  names(val) <- levels(group)
  
  # compute value for group 1
  cv1 <- mean(predicted[group == levels(group)[1]])
  
  # compute value for other groups
  for (i in 1:length(levels(group))) {
    val[i] <- mean(predicted[group == levels(group)[i]]) / cv1
  }
  
  return(val)
}



#' Demographic Parity
#'
#' This function computes the Demographic Parity metric (Calders and Verwer 2010)
#'
#' 
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return DP metric
#' @export
#' @examples
#' df = fairness::compas
#' dem_parity(df$score, df$race, "Caucasian")
dem_parity <- function(predicted, group, cutoff = 0.5, base = NULL) {
  
  # check lengths
  if (length(predicted) != length(group)) {
    stop("Predictions and groups must be of the same length")
  }
  
  # convert types
  group     <- as.factor(group) 
  predicted <- as.numeric(predicted >= cutoff)
  
  # relevel group
  if (is.null(base)) {base <- levels(group)[1]}
  group <- relevel(group, base)
  
  # placeholder
  val <- rep(NA, length(levels(group)))
  names(val) <- levels(group)
  
  # compute value for gorup 1
  cv1 <- mean(predicted[group == levels(group)[1]])
  
  # compute value for other groups
  for (i in 1:length(levels(group))) {
    val[i] <- 1 - (mean(predicted[group == levels(group)[i]]) - cv1)
  }
  
  return(val)
}



#' FPR Parity
#'
#' This function computes the False Positive Rate Parity metric (Chouldechova 2017)
#' 
#' 
#' @param actuals Vector of actual target values
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return FPR Parity metric
#' @examples
#' df = fairness::compas
#' fpr_parity(df$label_value, df$score, df$race, "Caucasian")
#' @export
fpr_parity <- function(actuals, predicted, group, cutoff = 0.5, base = NULL) {
  
  # check lengths
  if ((length(actuals) != length(predicted)) | (length(actuals) != length(group))) {
    stop("Actuals, predictions and groups must be of the same length")
  }
  
  # convert types
  group     <- as.factor(group) 
  actuals   <- as.numeric(actuals)
  predicted <- as.numeric(predicted >= cutoff)
  
  # relevel group
  if (is.null(base)) {base <- levels(group)[1]}
  group <- relevel(group, base)
  
  # placeholder
  val <- rep(NA, length(levels(group)))
  names(val) <- levels(group)
  
  # compute value for group 1
  fpr1 <- sum(actuals == 0 & predicted == 1 & group == levels(group)[1]) / 
    sum(actuals == 0 & group == levels(group)[1])
  
  # compute value for other groups
  for (i in 1:length(levels(group))) {
    fpri <- sum(actuals == 0 & predicted == 1 & group == levels(group)[i]) / 
      sum(actuals == 0 & group == levels(group)[i])
    val[i] <- ifelse(fpr1 != 0, fpri / fpr1, 1)
  }
  
  return(val)
}



#' FNR Parity
#'
#' This function computes the False Negative Rate Parity metric (Chouldechova 2017)
#' 
#' 
#' @param actuals Vector of actual target values
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return FNR Parity metric
#' @examples
#' df = fairness::compas
#' fnr_parity(df$label_value, df$score, df$race, "Caucasian")
#' @export
fnr_parity <- function(actuals, predicted, group, cutoff = 0.5, base = NULL) {
  
  # check lengths
  if ((length(actuals) != length(predicted)) | (length(actuals) != length(group))) {
    stop("Actuals, predictions and groups must be of the same length")
  }
  
  # convert types
  group     <- as.factor(group) 
  actuals   <- as.numeric(actuals)
  predicted <- as.numeric(predicted >= cutoff)
  
  # relevel group
  if (is.null(base)) {base <- levels(group)[1]}
  group <- relevel(group, base)
  
  # placeholder
  val <- rep(NA, length(levels(group)))
  names(val) <- levels(group)
  
  # compute value for gorup 1
  fnr1 <- sum(actuals == 1 & predicted == 0 & group == levels(group)[1]) / 
    sum(actuals == 1 & group == levels(group)[1])
  
  # compute value for other groups
  for (i in 1:length(levels(group))) {
    fnri <- sum(actuals == 1 & predicted == 0 & group == levels(group)[i]) / 
      sum(actuals == 1 & group == levels(group)[i])
    val[i] <- ifelse(fnr1 != 0, fnri / fnr1, 1)
  }
  
  return(val)
}



#' PPV Parity
#'
#' This function computes the Positive Predicted Value Parity metric (see Aeuquitas bias audit toolkit)
#' 
#' 
#' @param actuals Vector of actual target values
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return FNR Parity metric
#' @examples
#' df = fairness::compas
#' ppv_pariy(df$label_value, df$score, df$race, "Caucasian")
#' @export
ppv_parity <- function(actuals, predicted, group, cutoff = 0.5, base = NULL) {
  
  # check lengths
  if ((length(actuals) != length(predicted)) | (length(actuals) != length(group))) {
    stop("Actuals, predictions and groups must be of the same length")
  }
  
  # convert types
  group     <- as.factor(group) 
  actuals   <- as.numeric(actuals)
  predicted <- as.numeric(predicted >= cutoff)
  
  # relevel group
  if (is.null(base)) {base <- levels(group)[1]}
  group <- relevel(group, base)
  
  # placeholder
  val <- rep(NA, length(levels(group)))
  names(val) <- levels(group)
  
  # compute value for group 1
  ppv1 <- sum(actuals == 1 & predicted == 1 & group == levels(group)[1]) / 
    sum(predicted == 1 & group == levels(group)[1])
  
  # compute value for other groups
  for (i in 1:length(levels(group))) {
    ppvi <- sum(actuals == 1 & predicted == 1 & group == levels(group)[i]) / 
      sum(predicted == 1 & group == levels(group)[i])
    val[i] <- ifelse(ppv1 != 0, ppvi / ppv1, 1)
  }
  
  return(val)
}



#' NPV Parity
#'
#' This function computes the Negative Positive Value Parity metric (see Aeuquitas bias audit toolkit)
#' 
#' 
#' @param actuals Vector of actual target values
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return FNR Parity metric
#' @examples
#' df = fairness::compas
#' npv_parity(df$label_value, df$score, df$race, "Caucasian")
#' @export
npv_parity <- function(actuals, predicted, group, cutoff = 0.5, base = NULL) {
  
  # check lengths
  if ((length(actuals) != length(predicted)) | (length(actuals) != length(group))) {
    stop("Actuals, predictions and groups must be of the same length")
  }
  
  # convert types
  group     <- as.factor(group) 
  actuals   <- as.numeric(actuals)
  predicted <- as.numeric(predicted >= cutoff)
  
  # relevel group
  if (is.null(base)) {base <- levels(group)[1]}
  group <- relevel(group, base)
  
  # placeholder
  val <- rep(NA, length(levels(group)))
  names(val) <- levels(group)
  
  # compute value for group 1
  npv1 <- sum(actuals == 0 & predicted == 0 & group == levels(group)[1]) / 
    sum(predicted == 0 & group == levels(group)[1])
  
  # compute value for other groups
  for (i in 1:length(levels(group))) {
    npvi <- sum(actuals == 0 & predicted == 0 & group == levels(group)[i]) / 
      sum(predicted == 0 & group == levels(group)[i])
    val[i] <- ifelse(npv1 != 0, npvi / npv1, 1)
  }
  
  return(val)
}



#' Accuracy Parity
#'
#' This function computes the Accuracy Parity metric (Friedler et al. 2018)
#' 
#' 
#' @param actuals Vector of actual target values
#' @param predicted Vector of predicted target values
#' @param group Sensitive group (binary or factor)
#' @param cutoff Cutoff for rounding the probabilities
#' @return Accuracy Parity metric
#' @examples
#' df = fairness::compas
#' acc_parity(df$label_value, df$score, df$race, "Caucasian")
#' @export
acc_parity <- function(actuals, predicted, group, cutoff = 0.5, base = NULL) {
  
  # check lengths
  if ((length(actuals) != length(predicted)) | (length(actuals) != length(group))) {
    stop("Actuals, predictions and groups must be of the same length")
  }
  
  # convert types
  group     <- as.factor(group) 
  actuals   <- as.numeric(actuals)
  predicted <- as.numeric(predicted >= cutoff)
  
  # relevel group
  if (is.null(base)) {base <- levels(group)[1]}
  group <- relevel(group, base)
  
  # placeholder
  val <- rep(NA, length(levels(group)))
  names(val) <- levels(group)
  
  # compute value for group 1
  ac1 <- sum(actuals[group == levels(group)[1]] == predicted[group == levels(group)[1]]) / 
    sum(group == levels(group)[1])
  
  # compute value for other groups
  for (i in 1:length(levels(group))) {
    aci <- sum(actuals[group == levels(group)[i]] == predicted[group == levels(group)[i]]) / 
      sum(group == levels(group)[i])
    val[i] <- ifelse(ac1 != 0, aci / ac1, 1)
  }
  
  return(val)
}