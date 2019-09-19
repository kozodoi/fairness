#' Modified COMPAS dataset
#'
#' @description
#' \code{\link{compas}} is a landmark dataset to study algorithmic (un)fairness. This data was used to
#' predict recidivism (whether a criminal will reoffend or not) in the USA. The tool was meant to overcome
#' human biases and offer an algorithmic, fair solution to predict recidivism in a diverse population.
#' However, the algorithm ended up propagating existing social biases and thus, offered an unfair algorithmic
#' solution to the problem. In this dataset, a model to predict recidivism has already been fit and predicted
#' probabilities and predicted status (yes/no) for recidivism have been concatenated to the original data.
#'
#' @format A data frame with 6172 rows and 9 variables:
#' \describe{
#'   \item{Two_yr_Recidivism}{factor, yes/no for recidivism or no recidivism. This is the outcome or target in this dataset}
#'   \item{Number_of_Priors}{numeric, number of priors, normalized to mean = 0 and standard deviation = 1}
#'   \item{Age_Above_FourtyFive}{factor, yes/no for age above 45 years or not}
#'   \item{Age_Below_TwentyFive}{factor, yes/no for age below 25 years or not}
#'   \item{Female}{factor, female/male for gender}
#'   \item{Misdemeanor}{factor, yes/no for having recorded misdemeanor(s) or not}
#'   \item{ethnicity}{factor, Caucasian, African American, Asian, Hispanic, Native American or Other}
#'   \item{probability}{numeric, predicted probabilities for recidivism, ranges from 0 to 1}
#'   \item{predicted}{numeric, predicted values for recidivism, 0/1 for no/yes}
#' }
#'
#' @source The dataset is downloaded from Kaggle \url{https://www.kaggle.com/danofer/compass} and has undergone modifications (e.g. ethnicity was originally encoded using one-hot encoding, number or priors have been normalized, variables have been renamed, prediction model was fit and predicted probabilities and predicted status were concatenated to the original dataset).
"compas"
