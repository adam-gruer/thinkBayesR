source("Pmf.R")

pmf <-  Pmf$new()
pmf$Set('Bowl 1', 0.5)
pmf$Set('Bowl 2', 0.5)

pmf$Mult('Bowl 1', 0.75)
pmf$Mult('Bowl 2', 0.5)

pmf$Normalize()

pmf$Prob("Bowl 1")

Cookie <- R6Class("Cookie",
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
          
          mixes = hash("Bowl 1" = hash("vanilla"= 0.75, "chocolate" = 0.25),
                       "Bowl 2" = hash("vanilla"= 0.5, "chocolate" = 0.5))
        )
        
)

hypos <- c("Bowl 1","Bowl 2")
pmf = Cookie$new(hypos)

pmf$Update("vanilla")

pmf$d
