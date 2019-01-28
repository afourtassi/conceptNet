quantile_func <- function(x, probs){
  if (anyNA(x)){
    return(NA)
  } else {
    return(quantile(x, probs = probs))
  }
}
