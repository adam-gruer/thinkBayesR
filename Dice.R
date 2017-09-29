source("Suite.R")

Dice <- R6Class("Dice",
                inherit = Suite,
                public = list(
                  Likelihood = function(data, hypo){
                    if (hypo < data){
                      0
                    } else {
                      1.0 / as.numeric(hypo)
                    }
                  }
                ))

suite <- Dice$new(c(4, 6, 8, 12, 20))
suite
suite$Update(6)
suite
