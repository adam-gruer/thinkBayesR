as.data.frame.DictWrapper <- function(x, ... , stringsAsFactors = FALSE){
  if (x$numeric_values) {
    a <- as.numeric(keys(x$d))
  } else {
    a <- keys(x$d)
  }
  b <-  values(x$d)
  
  df <- data.frame(values = a, probabilities = b, stringsAsFactors = stringsAsFactors)
  if (x$numeric_values){
      df[order(df$values),] 
  } else {
      df
    } 
  
  
}
as.data.frame(suite)

#Create generic for Percentile
Percentile <- function(x, percentage,...){
  UseMethod("Percentile")
}

#Method where x has the class Pmf
Percentile.Pmf <-  function(x, percentage,...){
  p <- percentage / 100.0
  total = 0
  for (item in x$Items()){
    total <- total + item[[2]] 
  if (total >= p){
    item[[1]]
  }
  }
}
