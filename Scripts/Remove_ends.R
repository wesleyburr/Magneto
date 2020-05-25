Remove_ends <- function(x){
  for(j in 1:2){
    testing <- (2/4)*nrow(x)
    numb_na <- 0
    if(j == 2){
      x[,"y"] <- rev(x[,"y"])
    }
    #End of Line
    for(i in testing:nrow(x)){
      if(is.na(x[i,"y"])){
        numb_na <- numb_na +1
      }else{
        numb_na = 0
        next
      }
      if(numb_na >= 40){
        x[i:nrow(x),"y"] <- NA
        break
      }
    }
  }
  x[,"y"] <- rev(x[,"y"])
  x
}
