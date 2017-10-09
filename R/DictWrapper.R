
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
       
      ))

