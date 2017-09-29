library(R6)
library(hash)

DictWrapper <- R6Class("DictWrapper",
      public = list(
        
        name = NULL,
        d = NULL,
        
        log = FALSE,
        
        numeric_values = NULL,
        
        initialize = function(values = NULL, name = ""){
          self$d <- hash()
          self$name <- name
          self$numeric_values <- is.numeric(values)
          if(is.null(values)) return()
          self$InitSequence(values)
          if (length(self$d) > 0) self$Normalize()
        },
        
        InitSequence = function(values){
           values <- as.character(values)
           for (value in values)
            self$Set(value, 1)
          
        },
        
        Set = function(x, y = 0){
          self$d[[x]] <- y
        },
        
        Total = function(){
          sum(values(self$d))
        },
        
       Normalize = function(){
         
         total <- self$Total()
         values(self$d) <- values(self$d) / total
       },
       
       Values = function(){
              keys(self$d)
       },
       
       Items = function(){
            
            items <- lapply(keys(self$d), function(x){
              list(x, self$d[[x]] )
            })
            if(self$numeric_values){
              items[order(as.numeric(keys(self$d)))]
            } else {
              items
            }
           
       },
       Mult = function(x, factor){
       self$d[[x]] = self$d[[x]] * factor
       }, 
       print = function(){
         
         for (item in self$Items()){
           cat(item[[1]],item[[2]],"\n")
         }
       }
      ))

