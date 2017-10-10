
Train <- R6::R6Class("Train",
                 inherit = Suite,
                 public = list(
                   Likelihood = function(data, hypo){
                     
                     ifelse(hypo < data, 0, 1 / hypo)
                   }
                 ))
hypos <- 1:1000
suite <- Train$new(hypos)
suite$Update(60)


#df <- data.frame(suite)
plot(suite$Items(),type="l")
suite$Mean()

#Different upper bounds and more data
data <- c(60, 30, 90)
upperBounds <- c(500, 1000, 2000)
for(bound in upperBounds){
  suite <- Train$new(1:bound)
  for(dat in data){
    suite$Update(dat)
  }
  cat(bound, suite$Mean(),"\n")
}


Train <- R6::R6Class("Train",
                 inherit = Suite,
                 public = list(
                   InitSequence = function(values){
                     
                     #values <- as.character(values)
                     
                     for (value in values)
                       self$Set(value, value ^ -1)
                     
                   },
                   Likelihood = function(data, hypo){
                      
                     ifelse(hypo < data, 0, 1 / hypo)
                   }
                 ))

hypos <- 1:1000
suite <- Train$new(hypos)
suite$Update(60)

 
plot(suite$Items(),type="l")
suite$Mean()

data <- c(60, 30, 90)
upperBounds <- c(500, 1000, 2000)
for(bound in upperBounds){
  suite <- Train$new(1:bound)
  for(dat in data){
    suite$Update(dat)
  }
  cat(bound, suite$Mean(),"\n")
}

## Credible interval
data <- c(60, 30, 90)
suite <- Train$new(1:1000)
for(dat in data){
  suite$Update(dat)
}

cat(Percentile(suite,5), ",",Percentile(suite,95))

