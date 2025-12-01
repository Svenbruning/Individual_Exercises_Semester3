library(testthat)
source("believe.R")

# Test 1: normale string
test_that("counts vowels in a normal string", {
  expect_equal(count_vowels("hello world"), 3)
})

# Test 2: hoofdletters moeten ook meetellen
test_that("counts uppercase vowels", {
  expect_equal(count_vowels("AEIOU"), 5)
})

# Test 3: geen klinkers
test_that("returns 0 when no vowels are present", {
  expect_equal(count_vowels("rhythms"), 0)
})

# Test 4: lege string
test_that("handles empty string", {
  expect_equal(count_vowels(""), 0)
})

# Test 5: verkeerde input type
test_that("errors on non-character input", {
  expect_error(count_vowels(123))
  expect_error(count_vowels(TRUE))
  expect_error(count_vowels(c("a", "b")))
})
