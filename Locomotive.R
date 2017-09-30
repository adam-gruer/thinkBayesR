source("Suite.R")

Train <- R6Class("Train",
                 inherit = Suite,
                 public = list(
                   Likelihood = function(data, hypo){
                     hypo <- as.numeric(hypo)
                     ifelse(hypo < data, 0, 1 / hypo)
                   }
                 ))
hypos <- 1:1000
suite <- Train$new(hypos)
suite$Update(60)
class(suite)

x <- as.numeric(keys(suite$d))
y <-  values(suite$d)

df <- data.frame(x = x, y = y)
plot(df[order(df$x),],type="l")
