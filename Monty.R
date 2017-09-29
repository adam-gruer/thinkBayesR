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
data <- "B"
pmf$Update(data)

for (item in pmf$Items()){
  cat(item[[1]],item[[2]],"\n")
}
