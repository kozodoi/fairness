context("Datasets")
library(fairness)

test_that("no warning when loading compas", {
  expect_warning(data("compas"), NA)})

data("compas")

test_that("is compas a data frame", {
  expect_true(is.data.frame(compas))})
test_that("number of cols in compas", {
  expect_equal(ncol(compas), 9)})
test_that("number of rows in compas", {
  expect_equal(nrow(compas), 6172)})

test_that("no warning when loading germancredit", {
  expect_warning(data("germancredit"), NA)})

data("germancredit")

test_that("is germancredit a data frame", {
  expect_true(is.data.frame(germancredit))})
test_that("number of cols in germancredit", {
  expect_equal(ncol(germancredit), 23)})
test_that("number of rows in germancredit", {
  expect_equal(nrow(germancredit), 1000)})
