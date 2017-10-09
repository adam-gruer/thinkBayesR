

Dice <- R6::R6Class("Dice",
                inherit = Suite,
                public = list(
                  Likelihood = function(data, hypo){
                    hypo <- as.numeric(hypo)
                    if (hypo < data){
                      0
                    } else {
                      1.0 / hypo
                    }
                  }
                ))

suite <- Dice$new(c(4, 6, 8, 12, 20))
suite$Items()
suite
suite$Update(6)
suite
for (roll in c(6, 8, 7, 7, 5, 4)){
  suite$Update(roll)
}
suite
