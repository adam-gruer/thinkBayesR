as.data.frame.DictWrapper <- function(x, ... , stringsAsFactors = FALSE){
  if (x$numeric_values) {
    a <- as.numeric(hash::keys(x$d))
  } else {
    a <- hash::keys(x$d)
  }
  b <-  hash::values(x$d)
  
  df <- data.frame(values = a, probabilities = b, stringsAsFactors = stringsAsFactors)
  if (x$numeric_values){
      df[order(df$values),] 
  } else {
      df
    } 
  
  
}

length.Dict <- function(x){
  length(x$values)
}

length.DictWrapper <- function(x){
  length(x$Values())
}

#Create generic for Percentile
Percentile <- function(x, percentage,...){
  UseMethod("Percentile")
}

#Method where x has the class Pmf
Percentile.Pmf <-  function(x, percentage,...){
  p <- percentage / 100.0
  suite$Values()[findInterval(p,cumsum(suite$Probs()))+1] 
}


