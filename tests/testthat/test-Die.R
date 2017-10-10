context("Die")

test_that("the Die problem in Chapter 5 can be implemented", {
  Die <- R6::R6Class("Die",
                     inherit = Pmf,
                     public = list(
                       initialize = function(sides){
                         super$initialize()
                         self$Set(seq(sides),1)
                         self$Normalize()
                       }
                     ))
  
  d6 = Die$new(6)
  df <- data.frame(values = seq(6),
                   probs =    rep(1/6, 6),
                   stringsAsFactors = FALSE)
  
  expect_equal(d6$Items(), df, tolerance = 1e-04)
 
})
