source("Pmf.R")

Monty <- R6Class("Monty",
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
pmf
data <- "B"
pmf$Update(data)
pmf
