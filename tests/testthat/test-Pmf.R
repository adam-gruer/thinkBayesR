context("Pmf")
pmf <-  Pmf$new()


test_that("Pmf values and probabilities can be set", {
  pmf$Set('Bowl 1', 0.5)
  pmf$Set('Bowl 2', 0.5)
  expect_equal(pmf$d$values[1], "Bowl 1")
  expect_equal(pmf$d$values[2], "Bowl 2")
  expect_equal(pmf$d$probs[1], 0.5)
  expect_equal(pmf$d$probs[2], 0.5)
})

test_that("Prob returns the probability for a given values (i.e. hypothesis)",{
  expect_equal(pmf$Prob("Bowl 1"), 0.5)
  expect_equal(pmf$Prob("Bowl 2"), 0.5)
  
})

test_that("Mult multiplies existing probability by a factor",{
  pmf$Mult('Bowl 1', 0.75)
  pmf$Mult('Bowl 2', 0.5)
  expect_equal(pmf$Prob("Bowl 1"), 0.375)
  expect_equal(pmf$Prob("Bowl 2"), 0.25)
})


test_that("Normalize normalizes all probabilties so that they sum to 1",{
  pmf$Normalize()
  
  expect_equal(pmf$Total(), 1)
  expect_equal(pmf$Prob("Bowl 1"), 0.6)
  expect_equal(pmf$Prob("Bowl 2"), 0.4)
  
  
})