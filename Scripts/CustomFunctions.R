#Used in part 5
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



# Used in part 6
first_point <- function(x,w = 1){
  for(i in w:max(x[,"x"])){
    test <- x[i,"y"]
    if(is.na(test) == FALSE){
      y <- x[i,"y"]
      x <- x[i,"x"]
      return(cbind(y,x))
    }
  }
}

last_point <- function(x){
  na_count <- 0
  for(i in round((1/2)*nrow(x)):nrow(x)){
    test <- x[i,"y"]
    if(is.na(test) == TRUE){
      na_count <- na_count + 1
      if(na_count == 25){
        y <- x[i-25,"y"]
        x <- x[i-25,"x"]
        return(cbind(y,x))
      }
    }
    if(is.na(test)==FALSE){
      na_count <-0
    }
  }
}




linear_sub <- function(trace, nonNaStart = 1, nonNaEnd = 2){
  tally <- 0
  jj <- 0
  flag111 <- FALSE
  try(for(i in nonNaStart:nonNaEnd){
    if(is.na(trace[i])==FALSE){
      flag111 <- TRUE
    }
    if(flag111 == TRUE & is.na(trace[i])==TRUE){
      tally <- tally +1
    }
    if(tally >= 10 & is.na(trace[i])==FALSE){
      gap_end <- trace[i]
      gap_start <- trace[i-1-tally]
      if(gap_start > gap_end){
        for(j in (i-tally):(i-1)){
          jj <- jj + 1
          trace[j] <- -((gap_start - gap_end)/(i - (i-1-tally)))*jj + gap_start
        }
      }else{
        if(gap_start < gap_end){
          for(j in (i-tally):(i-1)){
            jj <- jj + 1
            trace[j] <- ((gap_end-gap_start)/(i - (i-1-tally)))*jj + gap_start
          }
        }else{
          if(gap_start == gap_end){
            for(j in (i-tally):(i-1)){

              trace[j] <- gap_start
            }
          }}
      }
      tally <- 0
      jj <- 0
    }
  })
  trace
}

