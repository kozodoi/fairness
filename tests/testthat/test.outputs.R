context("Outputs")
library(fairness)

data("compas")

# test for number of output items when probability is specified
test_that("number of outputs in acc_parity", {
  expect_equal(length(acc_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                          probs = "probability", preds = NULL, preds_levels = c("no", "yes"),
                          cutoff = 0.5, base = "Caucasian")), 3)})

test_that("number of outputs in dem_parity", {
  expect_equal(length(dem_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                          probs = "probability", preds = NULL,
                          cutoff = 0.5, base = "Caucasian")), 3)})

test_that("number of outputs in equal_odds", {
  expect_equal(length(equal_odds(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                          probs = "probability", preds = NULL, preds_levels = c("no", "yes"),
                          cutoff = 0.5, base = "Caucasian")), 3)})

test_that("number of outputs in fnr_parity", {
  expect_equal(length(fnr_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                          probs = "probability", preds = NULL, preds_levels = c("no", "yes"),
                          cutoff = 0.5, base = "Caucasian")), 3)})

test_that("number of outputs in fpr_parity", {
  expect_equal(length(fpr_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                          probs = "probability", preds = NULL, preds_levels = c("no", "yes"),
                          cutoff = 0.5, base = "Caucasian")), 3)})

test_that("number of outputs in mcc_parity", {
  expect_equal(length(mcc_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                          probs = "probability", preds = NULL, preds_levels = c("no", "yes"),
                          cutoff = 0.5, base = "Caucasian")), 3)})

test_that("number of outputs in npv_parity", {
  expect_equal(length(npv_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                          probs = "probability", preds = NULL, preds_levels = c("no", "yes"),
                          cutoff = 0.5, base = "Caucasian")), 3)})

test_that("number of outputs in pred_rate_parity", {
  expect_equal(length(pred_rate_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                probs = "probability", preds = NULL, preds_levels = c("no", "yes"),
                                cutoff = 0.5, base = "Caucasian")), 3)})

test_that("number of outputs in prop_parity", {
  expect_equal(length(prop_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                           probs = "probability", preds = NULL,
                           cutoff = 0.5, base = "Caucasian")), 3)})

test_that("number of outputs in roc_parity", {
  expect_equal(length(roc_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                          probs = "probability", preds_levels = c("no", "yes"),
                          base = "Caucasian")), 4)})

test_that("number of outputs in spec_parity", {
  expect_equal(length(spec_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                           probs = "probability", preds = NULL, preds_levels = c("no", "yes"),
                           cutoff = 0.5, base = "Caucasian")), 3)})

# test for number of output items when predicted class is specified
test_that("number of outputs in acc_parity", {
  expect_equal(length(acc_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                 probs = NULL, preds = "predicted", preds_levels = c("no", "yes"),
                                 cutoff = 0.5, base = "Caucasian")), 2)})

test_that("number of outputs in dem_parity", {
  expect_equal(length(dem_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                 probs = NULL, preds = "predicted",
                                 cutoff = 0.5, base = "Caucasian")), 2)})

test_that("number of outputs in equal_odds", {
  expect_equal(length(equal_odds(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                 probs = NULL, preds = "predicted", preds_levels = c("no", "yes"),
                                 cutoff = 0.5, base = "Caucasian")), 2)})

test_that("number of outputs in fnr_parity", {
  expect_equal(length(fnr_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                 probs = NULL, preds = "predicted", preds_levels = c("no", "yes"),
                                 cutoff = 0.5, base = "Caucasian")), 2)})

test_that("number of outputs in fpr_parity", {
  expect_equal(length(fpr_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                 probs = NULL, preds = "predicted", preds_levels = c("no", "yes"),
                                 cutoff = 0.5, base = "Caucasian")), 2)})

test_that("number of outputs in mcc_parity", {
  expect_equal(length(mcc_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                 probs = NULL, preds = "predicted", preds_levels = c("no", "yes"),
                                 cutoff = 0.5, base = "Caucasian")), 2)})

test_that("number of outputs in npv_parity", {
  expect_equal(length(npv_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                 probs = NULL, preds = "predicted", preds_levels = c("no", "yes"),
                                 cutoff = 0.5, base = "Caucasian")), 2)})

test_that("number of outputs in pred_rate_parity", {
  expect_equal(length(pred_rate_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                       probs = NULL, preds = "predicted", preds_levels = c("no", "yes"),
                                       cutoff = 0.5, base = "Caucasian")), 2)})

test_that("number of outputs in prop_parity", {
  expect_equal(length(prop_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                  probs = NULL, preds = "predicted",
                                  cutoff = 0.5, base = "Caucasian")), 2)})

test_that("number of outputs in spec_parity", {
  expect_equal(length(spec_parity(data = compas, outcome = "Two_yr_Recidivism", group = "ethnicity",
                                  probs = NULL, preds = "predicted", preds_levels = c("no", "yes"),
                                  cutoff = 0.5, base = "Caucasian")), 2)})

