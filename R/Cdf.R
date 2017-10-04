Cdf <- R6::R6Class("Cdf",
#Represents a cumulative distribution function.
#Attributes:
#xs: sequence of values
#ps: sequence of probabilities
#name: string used as a graph label.

     public = list(
          xs = NULL,
          ps = NULL,
          name = "",
          initialize = function( xs=NULL, ps=NULL, name=""){
              self$xs  <-  xs
              self$ps  <-  ps
              self$name <- name
              },
          Value = function(p){
            if(p < 0 || p > 1 ){
              stop("Probability p must be in range 0 : 1")
            }
            if(p == 0) {return(self$xs[1])}
            if(p == 1) {return(self$xs[length(self$xs)])}
            index <-  findInterval(p, self$ps)
            if(p == self$ps[index]){
              return(self$xs[index])
              }
            else {
              return(self$xs[index+1])
            }
          }
))