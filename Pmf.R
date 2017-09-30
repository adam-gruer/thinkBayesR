source("DictWrapper.R")

Pmf <- R6Class("Pmf",
               inherit = DictWrapper,
               public = list(
                 Prob = function(x, default=0){
                   ifelse(has.key(x,self$d), self$d[[x]],default)
                 },
                 Mean = function(){
                   items <- self$Items()
                   l <- length(items)
                   x <- vector("numeric",l)
                   p <- x
                   for(i in 1:l){
                     x[i] <- items[[i]][[1]]
                     p[i] <- items[[i]][[2]]
                   }
                   sum(p * x)
                 }
               ))


