Dict <- R6::R6Class("Dict",
                    public = list(
                      initialize = function(values = numeric(), probs = numeric){
                        l = length(values)
                        private$checkDuplicates(values)
                        
                        
                        
                      }
                    ),
                     private = list(
                      dict = NULL,
                      check_duplicates = function(values){
                        if(anyDuplicated(values)){
                          stop("No duplicate values are allowed", call. = FALSE)
                        }
                      }
                      
                      
                    )
                    )