source("DictWrapper.R")

Pmf <- R6Class("Pmf",
               inherit = DictWrapper,
               public = list(
                 Prob = function(x, default=0){
                   ifelse(has.key(x,self$d), self$d[[x]],default)
                 },
                 Mean = function(){
                   x <- as.numeric(keys(self$d))
                   p <- values(self$d)
                   sum(p * x)
                 }
               ))


