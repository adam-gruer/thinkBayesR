library(R6)
library(hash)

DictWrapper <- R6Class("DictWrapper",
      public = list(
        
        name = NULL,
        d = NULL,
        
        log = FALSE,
        
        initialize = function(values = NULL, name = ""){
          self$d <- hash()
          self$name <- name
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
            lapply(keys(self$d), function(x){
              list(x, self$d[[x]] )
            })
         
       },
       Mult = function(x, factor){
       self$d[[x]] = self$d[[x]] * factor
       }
      ))

x <- DictWrapper$new(letters[1:3])
x$d
x$Mult("a",2)
