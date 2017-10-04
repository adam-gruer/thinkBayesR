Dict <- R6::R6Class("Dict",
                    public = list(
                      initialize = function(values = numeric(), probs = numeric()){
                       
                         private$l <-  length(values)
                        
                         self$add_items(values, probs)
                        
                        
                      },
                      add_items =  function(values = numeric(), probs = numeric()){
                        
                        private$dict$values <- private$check_duplicates(
                                                     c(private$dict$values,
                                                    values))
                        
                        private$dict$probs <- c(private$dict$probs,probs)
                        
                        },
                      print = function(){
                        
                      }
                    ),
                     private = list(
                      dict = list(values = numeric(),
                                  probs = numeric()
                                  ),
                      l = NULL,
                      check_duplicates = function(values,...){
                        if(anyDuplicated(values,nmax = private$l,...)){
                          stop("No duplicate values are allowed", call. = FALSE)
                          invisible(values)
                        }
                      }
                    )
                    )