
Pmf <- R6::R6Class("Pmf",
               inherit = DictWrapper,
               public = list(
                 Prob = function(x, default=0){
                   ifelse(hash::has.key(x,self$d), self$d[[x]],default)
                 },
                 Mean = function(){
                   x <- as.numeric(hash::keys(self$d))
                   p <- hash::values(self$d)
                   sum(p * x)
                 }
               ))


