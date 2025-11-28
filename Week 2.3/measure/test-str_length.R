library(stringr)
library(testthat)

# Test 1: simpele string
test_that("str_length works for basic strings", {
  expect_equal(str_length("abc"), 3)
})

# Test 2: lege string en whitespace
test_that("str_length counts empty strings and spaces", {
  expect_equal(str_length(""), 0)
  expect_equal(str_length(" "), 1)
})

# Test 3: niet-alfabetische tekens
test_that("str_length handles punctuations", {
  expect_equal(str_length("?!"), 2)
})

# Test 4: NA
test_that("str_length handles NA", {
  expect_true(is.na(str_length(NA)))
})

# Test 5: vector input
test_that("str_length works for vectors", {
  expect_equal(str_length(c("a", "bb")), c(1, 2))
})
