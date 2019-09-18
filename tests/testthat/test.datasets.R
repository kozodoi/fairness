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

files <- lapply(list.files(system.file('extdata', package = 'fairness'), full.names = TRUE), read.delim, sep=" ")
german <- files[[1]]

test_that("is german a data frame", {
  expect_true(is.data.frame(german))})
test_that("number of cols in german", {
  expect_equal(ncol(german), 23)})
test_that("number of rows in german", {
  expect_equal(nrow(german), 1000)})
