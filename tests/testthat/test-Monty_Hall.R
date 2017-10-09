context("Monty_Hall")

test_that("Monty Hall problem can be computed", {
 
Monty <- R6::R6Class("Monty",
                     inherit = Pmf,
                     public = list(
                       
                       Update = function(data){
                         for (hypo in self$Values()){
                           like <- self$Likelihood(data, hypo)
                           self$Mult(hypo, like)}
                         self$Normalize()},
                       
                       Likelihood = function(data,hypo) {
                         if(hypo == data) {
                           0
                         } else if (hypo == "A"){
                           0.5
                         } else {
                           1
                         }
                       }
                       
                       
                     )
                     
)

hypos <- LETTERS[1:3]
pmf <- Monty$new(hypos)
data <- "B"
pmf$Update(data)

expect_equal(pmf$Prob("A"), 0.3333, tolerance = 1e-04)
expect_equal(pmf$Prob("B"), 0, tolerance = 1e-04)
expect_equal(pmf$Prob("C"), 0.6667, tolerance = 1e-04)
 
})

