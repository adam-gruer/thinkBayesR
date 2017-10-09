
Pmf <- R6::R6Class("Pmf",
               inherit = DictWrapper,
               public = list(
                 Prob = function(x, default=0){
                   prob <- self$d$get_prob(x)
                   ifelse(length(prob) > 0 , prob, default)
                 },
                 Mean = function(){
                   if (is.numeric(self$d$values)){
                   sum(self$d$values * self$d$probs)
                    
                   } else {
                     stop("Values are not numeric", call. = FALSE)
                   }
                 }
               ))


