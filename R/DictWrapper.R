
DictWrapper <- R6::R6Class("DictWrapper",
      public = list(
        
        name = NULL,
        d = NULL,
        
        log = FALSE,
        #flag if original values were numeric
        numeric_values = NULL,
        
        initialize = function(values = NULL, name = ""){
          self$d <- Dict$new()
          self$name <- name
          self$numeric_values <- is.numeric(values)
          if(is.null(values)) return()
          self$InitSequence(values)
          if (length(self$d) > 0) self$Normalize()
        },
        
        InitSequence = function(values){
           
           self$Set(values, 1)
          
        },
        
        Set = function(x, y = 0){
          self$d$add_items(x,rep.int(y,length(x)))
        },
        
        Total = function(){
          sum(self$d$probs)
        },
        
       Normalize = function(){
         
         total <- self$Total()
         self$d$probs <- self$d$probs / total
       },
       
       Values = function(){
               self$d$values 
       },
       Probs =  function(){
         self$d$probs 
       },
       
       Items = function(){
            
            self$d$iter_items()
           
       },
       Mult = function(x, factor){
         
       self$d$probs[self$d$values == x] = self$d$probs[self$d$values == x] * factor
       invisible(self)
       }, 
       
       print = function(){
         
         self$d$print()
         }
       
      ),
      private = list(
        deep_clone = function(name, value) {
          # With x$clone(deep=TRUE) is called, the deep_clone gets invoked once for
          # each field, with the name and value.
          if (inherits(value, "R6")) {
            # field is R6 class, so create new instance
            Dict$new(value$values,value$probs)
          } else {
            # For all other fields, just return the value
            value
          }
        }
      ))



