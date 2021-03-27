context("Errors")
library(fairness)

data("compas")
compas$Two_yr_Recidivism_01 <- ifelse(compas$Two_yr_Recidivism == 'yes', 1, 0) 


# no preds OR probs defined
test_that("errors in acc_parity", {
  expect_error(acc_parity(data = compas, outcome = "Two_yr_Recidivism_01", group = "ethnicity",
                          probs = NULL, preds = NULL, 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in dem_parity", {
  expect_error(dem_parity(data = compas, outcome = "Two_yr_Recidivism_01", group = "ethnicity",
                          probs = NULL, preds = NULL,
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in equal_odds", {
  expect_error(equal_odds(data = compas, outcome = "Two_yr_Recidivism_01", group = "ethnicity",
                          probs = NULL, preds = NULL, 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in fnr_parity", {
  expect_error(fnr_parity(data = compas, outcome = "Two_yr_Recidivism_01", group = "ethnicity",
                          probs = NULL, preds = NULL, 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in fpr_parity", {
  expect_error(fpr_parity(data = compas, outcome = "Two_yr_Recidivism_01", group = "ethnicity",
                          probs = NULL, preds = NULL, 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in mcc_parity", {
  expect_error(mcc_parity(data = compas, outcome = "Two_yr_Recidivism_01", group = "ethnicity",
                          probs = NULL, preds = NULL, 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in npv_parity", {
  expect_error(npv_parity(data = compas, outcome = "Two_yr_Recidivism_01", group = "ethnicity",
                          probs = NULL, preds = NULL, 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in pred_rate_parity", {
  expect_error(pred_rate_parity(data = compas, outcome = "Two_yr_Recidivism_01", group = "ethnicity",
                                probs = NULL, preds = NULL, 
                                cutoff = 0.5, base = "Caucasian"))})

test_that("errors in prop_parity", {
  expect_error(prop_parity(data = compas, outcome = "Two_yr_Recidivism_01", group = "ethnicity",
                           probs = NULL, preds = NULL,
                           cutoff = 0.5, base = "Caucasian"))})

test_that("errors in spec_parity", {
  expect_error(spec_parity(data = compas, outcome = "Two_yr_Recidivism_01", group = "ethnicity",
                           probs = NULL, preds = NULL, 
                           cutoff = 0.5, base = "Caucasian"))})


# no outcome defined
test_that("errors in acc_parity, no outcome", {
  expect_error(acc_parity(data = compas, group = "ethnicity",
                          probs = NULL, preds = "predicted",
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in acc_parity, no outcome", {
  expect_error(acc_parity(data = compas,  group = "ethnicity",
                          probs = NULL, preds = "predicted", 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in equal_odds, no outcome", {
  expect_error(equal_odds(data = compas,  group = "ethnicity",
                          probs = NULL, preds = "predicted", 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in fnr_parity, no outcome", {
  expect_error(fnr_parity(data = compas,  group = "ethnicity",
                          probs = NULL, preds = "predicted", 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in fpr_parity, no outcome", {
  expect_error(fpr_parity(data = compas,  group = "ethnicity",
                          probs = NULL, preds = "predicted",
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in mcc_parity, no outcome", {
  expect_error(mcc_parity(data = compas,  group = "ethnicity",
                          probs = NULL, preds = "predicted", 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in npv_parity, no outcome", {
  expect_error(npv_parity(data = compas,  group = "ethnicity",
                          probs = NULL, preds = "predicted", 
                          cutoff = 0.5, base = "Caucasian"))})

test_that("errors in pred_rate_parity, no outcome", {
  expect_error(pred_rate_parity(data = compas,  group = "ethnicity",
                                probs = NULL, preds = "predicted",
                                cutoff = 0.5, base = "Caucasian"))})

test_that("errors in spec_parity, no outcome", {
  expect_error(spec_parity(data = compas,  group = "ethnicity",
                           probs = NULL, preds = "predicted", 
                           cutoff = 0.5, base = "Caucasian"))})

