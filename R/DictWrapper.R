
DictWrapper <- R6::R6Class("DictWrapper",
      public = list(
        
        name = NULL,
        d = NULL,
        
        log = FALSE,
        #flag if original values were numeric
        numeric_values = NULL,
        
        initialize = function(values = NULL, name = ""){
          self$d <- hash::hash()
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
         hash::values(self$d) <- hash::values(self$d) / total
       },
       
       Values = function(){
              hash::keys(self$d)
       },
       
       Items = function(){
            
            items <- lapply(hash::keys(self$d), function(x){
              list(x, self$d[[x]] )
            })
            if(self$numeric_values){
              items[order(as.numeric(hash::keys(self$d)))]
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

