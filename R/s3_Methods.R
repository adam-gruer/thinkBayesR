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


#Create generic for Percentile
Percentile <- function(x, percentage,...){
  UseMethod("Percentile")
}

#Method where x has the class Pmf
Percentile.Pmf <-  function(x, percentage,...){
  p <- percentage / 100.0
  total = 0
  probs <- x$Probs()

  
  for (i in seq_along(probs)){
    total <- total + probs[i]
  if (total >= p){
    val <- x$Values()[i]
    break
  }
  }
  val
}
