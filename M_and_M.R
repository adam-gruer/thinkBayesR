 

M_and_M <- R6::R6Class("M_and_M",
                   inherit = Suite,
                   private = list(
                     mix94 = list(brown = 30,
                                  yellow=20,
                                  red=20,
                                  green=10,
                                  orange=10,
                                  tan=10),
                     
                     mix96 = list(blue=24,
                                       green=20,
                                       orange=16,
                                       yellow=14,
                                       red=13,
                                       brown=13),
                     
                     hypoA = function(){list(bag1 = private$mix94,
                                          bag2 = private$mix96)},
                     
                     hypoB = function(){list(bag1 = private$mix96,
                                          bag2 = private$mix94)},
                     
                     hypotheses = function(){list(A=private$hypoA(),
                                               B=private$hypoB())
                       }),
                   public = list(
                     Likelihood = function(data, hypo){
                         bag <-  data[1]
                         color <- data[2]
                         private$hypotheses()[[hypo]][[bag]][[color]]
                     }
                   ))

suite = M_and_M$new(LETTERS[1:2])

suite$Update(c("bag1", "yellow"))
suite$Update(c("bag2", "green"))
suite
