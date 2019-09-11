#' Simplified COMPAS dataset
#'
#' @description
#' \code{\link{compas}} is a custom made dataframe that resembles a real-life clinical dataset.
#' The correlations between variables, the data means, SDs and ranges are realistic, but
#' the dataset is constructed by simulations and manual data input. The dataset contains
#' missing values (approximately 10\% missing overall), and values are missing in a realistic pattern.
#'
#' @format A data frame with 2500 rows and 12 variables:
#' \describe{
#'   \item{Two_yr_Recidivism}{factor, age, in years, 2.88\% missing - in general, age is not likely have lots of missing data in a realistic dataset, therefore only a few values are missing here randomly, e.g. due to mistakes in data input}
#'   \item{Number_of_Priors}{numeric, male=1 and female=2, 2.88\% missing - similar to age, sex information is also not likely have missing data in a realistic dataset, no values are missing here}
#'   \item{Age_Above_FourtyFive}{factor, waist circumference, in cm, 4.12\% missing - anthropometric data is easy to collect, therefore only a small fraction is missing here, often missing together with BMI, the other anthropometric variable}
#'   \item{Age_Below_TwentyFive}{factor, body mass index, in kg/m2, 4.16\% missing - anthropometric data is easy to collect, therefore only a small fraction is missing here, often missing together with waist, the other anthropometric variable}
#'   \item{Female}{factor, systolic blood pressure, in mmHg, 8.84\% missing - in a realistic fashion, SBP is almost always missing together with DBP}
#'   \item{Misdemeanor}{factor, diastolic blood pressure, in mmHg, 8.84\% missing - in a realistic fashion, DBP is almost always missing together with SBP}
#'   \item{ethnicity}{factor, blood fasting glucose concentration, in mmol/dl, 5.84\% missing - often missing together with other clinical variables}
#'   \item{probability}{numeric, predicted probabilities}
#'   \item{predicted}{numeric, predicted values, 0/1 for no/yes}
#' }
#'
#' @source The dataset is simulated and undergone manual configuration.
"compas"
