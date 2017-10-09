
Cookie <- R6::R6Class("Cookie",
                  inherit = Pmf,
                  public = list(
                    
                    Update = function(data){
                      for (hypo in self$Values()){
                        like <- self$Likelihood(data, hypo)
                        self$Mult(hypo, like)}
                      self$Normalize()},
                    
                    Likelihood = function(data,hypo) {
                      mix <-  self$mixes[[hypo]]
                      like <-  mix[[data]]
                    },
                    
                    mixes = list("Bowl 1" = c("vanilla"= 0.75, "chocolate" = 0.25),
                                 "Bowl 2" = c("vanilla"= 0.5, "chocolate" = 0.5))
                  )
                  
)

hypos <- c("Bowl 1","Bowl 2")
pmf = Cookie$new(hypos)

dataset <- c("vanilla","chocolate","vanilla")
for (data in dataset)
  pmf$Update(data)

pmf$d
