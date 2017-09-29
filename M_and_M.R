source("Suite.R")

M_and_M <- R6Class("M_and_M",
                   inherit = Suite,
                   public = list(
                     mix94 = hash(brown = 30,
                                  yellow=20,
                                  red=20,
                                  green=10,
                                  orange=10,
                                  tan=10),
                     
                     mix96 = hash(blue=24,
                                       green=20,
                                       orange=16,
                                       yellow=14,
                                       red=13,
                                       brown=13),
                     
                     hypoA = hash(bag1=self$mix94, bag2=self$mix96),
                     hypoB = hash(bag1=self$mix96, bag2=self$mix94),
                     
                     hypotheses = hash(A=self$hypoA, B=self$hypoB),
                     
                     Likelihood = function(data, hypo){
                         bag <-  data[1]
                         color <- data[2]
                         mix <- self$hypotheses[hypo][bag]
                         mix[color]
                         
                     }
                   ))

