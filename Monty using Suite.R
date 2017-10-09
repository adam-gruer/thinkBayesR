

Monty <- R6::R6Class("Monty",
                 inherit = Suite,
                 public = list(
                   Likelihood = function(data,hypo) {
                     if(hypo == data) {
                       0
                     } else if (hypo == "A"){
                       0.5
                     } else {
                       1
                     }
                   }
                 )) 
suite <-  Monty$new(LETTERS[1:3])
suite$Update('B')
suite