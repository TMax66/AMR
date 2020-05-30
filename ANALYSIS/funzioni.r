funz<-function(x){
  abs(as.numeric(as.factor(x))-2)
}


logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}