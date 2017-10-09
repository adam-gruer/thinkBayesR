
Suite <- R6::R6Class("Suite",
                 inherit = Pmf,
                 public = list(
                   Update = function(data){
                     for (hypo in self$Values()){
                     like <- self$Likelihood(data, hypo)
                     self$Mult(hypo, like)}
                     self$Normalize()
                     
                   },
                   
                    Likelihood  = function(){
                      stop("A Likelihood method needs to be implement for this subclass",
                           call. = FALSE)
                    }
                   
                 ))