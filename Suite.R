source("Pmf.R")

Suite <- R6Class("Suite",
                 inherit = Pmf,
                 public = list(
                   Update = function(data){
                     for (hypo in self$Values()){
                     like <- self$Likelihood(data, hypo)
                     self$Mult(hypo, like)}
                     self$Normalize()
                     
                   },
                   print = function(){
                     for (item in self$Items()){
                       cat(item[[1]],item[[2]],"\n")
                     }},
                    Likelihood  = function(){
                      stop("A Likelihood method needs to be implement for this subclass",
                           call. = FALSE)
                    }
                   
                 ))