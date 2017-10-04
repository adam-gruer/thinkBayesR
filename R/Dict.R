Dict <- R6::R6Class("Dict",
                    public = list(
                      initialize = function(values = numeric(), probs = numeric()){
                        private$l <-  length(values)
                        private$check_duplicates(values)
                        private$dict <- private$add_items(values, probs)
                        
                        
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
                      },
                      add_items =  function(values, probs){}
                      check_duplicates(c(private$dict$values))
                      
                    )
                    )