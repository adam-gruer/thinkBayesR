Dict <- R6::R6Class("Dict",
                    public = list(
                      values = numeric(),
                      probs = numeric(),
                      initialize = function(values = numeric(), probs = numeric()){
                       
                         private$l <-  length(values)
                         self$add_items(values, probs)
                        
                        
                      },
                      add_items =  function(values = numeric(), probs = numeric()){
                        
                        self$values <- private$check_duplicates(
                                                     c(self$values,
                                                    values))
                        
                        self$probs <- if (is.numeric(probs)) {
                                c(self$probs,probs) } else {
                                stop("Probabilities must be numeric", .call = FALSE)
                                }
                        
                        },
                      print = function(){
                        print(data.frame(values = self$values,
                                   probs = self$probs,
                                   stringsAsFactors = FALSE),
                              row.names = FALSE)
                      }
                    ),
                     private = list(

                     l = NULL,
                     check_duplicates = function(values,...){
                        if(anyDuplicated(values,nmax = private$l,...)){
                          stop("No duplicate values are allowed", call. = FALSE)
                          
                        } else invisible(values)
                      }
                    )
                    )