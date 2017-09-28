
Pmf <- R6::R6Class("Pmf", list(
   
  pmf = NULL,
  initialize = function(hypos = NULL){
    self$pmf <-  data.frame(values = hypos,
                            probs = rep(1,length(hypos)),
                            stringsAsFactors = "FALSE")
     
    self$Normalize()
  },
  Set = function(x, y = 0){
    
    self$pmf[self$pmf$values == x,]$probs <- y
  }
  , Normalize = function(){
    self$pmf$probs <- self$pmf$probs / sum(self$pmf$probs)
    
  }
  , Mult = function(x,factor){
    
    self$pmf[self$pmf$values == x,]$probs <- 
          self$pmf[self$pmf$values == x,]$probs * factor
    
  }
))

x <- Pmf$new(c("Bowl 1","Bowl 2"))
 

x$Mult("Bowl 1", 0.75)
x$Mult("Bowl 2", 0.5)
x$Normalize()
x$pmf

Cookie <- R6Class("Cookie", 
            inherit = Pmf,
            public = list(
              Update = function(data){
                self$pmf
                
              }
            )
                             
)
