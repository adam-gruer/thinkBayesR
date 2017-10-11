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


# add two Pmfs
`+.Pmf` <- function(x,y) {
  x_vals <- x$Values()
  y_vals <- y$Values()
  outer <- outer(x_vals,y_vals,"+")
  
  values <- c(outer[,1], outer[length(x_vals),-1])
  probs <- convolve(x$Probs(),rev(y$Probs()), type = "open")
  pmf <- Pmf$new()
  pmf$d$values <- values
  pmf$d$probs <- probs
  invisible(pmf)
  
}

