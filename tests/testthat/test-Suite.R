context("Suite")

test_that("Class inheriting from Suite updates correctly", {
  Monty <- R6::R6Class("Monty",
                       inherit = Suite,
                       public = list(
                         Likelihood = function(data,hypo) {
                           if(hypo == data) {
                             0
                           } else if (hypo == "A"){
                             0.5
                           } else {
                             1
                           }
                         }
                       )) 
  suite <-  Monty$new(LETTERS[1:3])
  suite$Update('B')
  
df <- data.frame(values = c("A","B","C"), probs = c(0.3333, 0, 0.6667), stringsAsFactors = FALSE)

  expect_equal(suite$Items(), df, tolerance = 1e-04)

})
