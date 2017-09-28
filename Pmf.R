source("DictWrapper.R")

Pmf <- R6Class("Pmf",
               inherit = DictWrapper,
               public = list(
                 Prob = function(x, default=0){
                   ifelse(has.key(x,self$d), self$d[[x]],default)
                 }
               ))


