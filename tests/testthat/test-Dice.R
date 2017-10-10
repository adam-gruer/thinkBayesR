context("Dice")

test_that("Dice problem can be correctly calculated", {
  Dice <- R6::R6Class("Dice",
                      inherit = Suite,
                      public = list(
                        Likelihood = function(data, hypo){
                          
                          if (hypo < data){
                            0
                          } else {
                            1.0 / hypo
                          }
                        }
                      ))
  
  suite <- Dice$new(c(4, 6, 8, 12, 20))
 suite$Update(6)

  df <- data.frame(values = c(4, 6, 8, 12, 20),
                   probs =     c(0,
                                 0.39215,
                                 0.29411,
                                 0.19607,
                                 0.11764),
                   stringsAsFactors = FALSE)
  
  expect_equal(suite$Items(), df, tolerance = 1e-04)
  
  for (roll in c(6, 8, 7, 7, 5, 4)){
    suite$Update(roll)
  }
  
  df$probs <- c(0,
                0,
                0.94324,
                0.05520,
                0.00154)
  
  expect_equal(suite$Items(), df, tolerance = 1e-04)
  
})
