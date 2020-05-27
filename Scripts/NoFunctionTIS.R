# Hello!
#Script by Mark Weygang, Trent University November 2017
#Uses work by Aaron Springford , Queens University. In particular -> Deconvolution, for time series smoothing
#The strategy is to separate the traces, and baselines with

##### Organize ######
library("tiff")
library("pracma")
library("multitaper")
library("robustbase") # for colMedians

source("~/Magneto2020/Scripts/BreakoutOptimization.R")
source("~/Magneto2020/Scripts/CustomFunctions.R")

## Setting the Scene ##
print("===== Loading Image, and applying Decon. =====")
# Test Images
# mag1 <- readTIFF("Images/AGC-D-18980111-18980113.tif")
# #mag1 <- readTIFF("Images/AGC-D-18980113-18980115.tif")
# #mag1 <- readTIFF("Images/AGC-D-19000308-19000310.tif")
# mag1 <- readTIFF("Images/AGC-D-18980228-18980302.tif")
# #mag1 <- readTIFF("Images/AGC-D-18980228-18980302.tif") #Single Intersection, does not cross (lower)
# #mag1 <- readTIFF("Images/AGC-D-18980529-18980531.tif") #Single Intersection, barely (lower)
# mag1 <- readTIFF("Images/AGC-D-19071125-19071127.tif")
# mag1 <- readTIFF("Images/19040830-19071124/AGC-D-19040830-19071124/AGC-D-19050609-19050611.tif")
# mag1 <- readTIFF("Images/19040830-19071124/AGC-D-19040830-19071124/AGC-D-19051110-19051112.tif")
# mag1 <- readTIFF("Images/19040830-19071124/AGC-D-19040830-19071124/AGC-D-19050609-19050611.tif")
#Obtaining Data for loop ==============================================================

# Read in image data

file.names <- dir("Images/18980111-19010529/AGC-D-18980111-19010529", pattern =".tif")
image_import <- function(image){readTIFF(sprintf("Images/18980111-19010529/AGC-D-18980111-19010529/%s",image))}

file.names <- dir("Images/19010530-19040829/AGC-D-19010530-19040829", pattern =".tif")
image_import <- function(image){readTIFF(sprintf("Images/19010530-19040829/AGC-D-19010530-19040829/%s",image))}

file.names <- dir("Images/19040830-19071124/AGC-D-19040830-19071124", pattern =".tif")
image_import <- function(image){readTIFF(sprintf("Images/19040830-19071124/AGC-D-19040830-19071124/%s",image))}

file.names <- dir("Images/19071125-19110201/AGC-D-19071125-19110201", pattern =".tif")
image_import <- function(image){readTIFF(sprintf("Images/19071125-19110201/AGC-D-19071125-19110201/%s",image))}

time_group <- 1900 # or 1900 : has to do with the brightness of the image



for ( O in 1:length(file.names)){
  O <- 18 # 18 is a good example, 23 breaks, 29 has no first/second peak, 32-33 only 1 trace, 39-41-44-58 have to find both peaks (18980111-19010529)
  #mag1 <- readTIFF("AGC-D-19041114-19041116.tif")
  mag1 <- image_import(image = file.names[O])
  ncol_mag1 <- ncol(mag1)
  nrow_mag1 <- nrow(mag1)

  # Some images have been scaned vertically, therefore to remain consistent..
  if(ncol_mag1 < nrow_mag1){
    mag1 <- apply(t(mag1),2,rev)
  }
  #

  filename <- file.names[O]

  mag2 <- t( apply( mag1, MARGIN = 1, FUN = deconvGauss, sig = 10, kern.trunc = 0.05, nw = 3 ) )
  print("")

  ## Now Go Seperate Ways ##

  ## Preprocessing for mag1 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  print("===== Preprocessing for mag1 =====")


  # Some images also have very bright backgrounds, example 19010530-19040829 onwards. Do not use prior to those images at this time

  mag1[mag1<(quantile(mag1,0.95))] <- 0
  mag1[mag1>0] <- 1

  writeTIFF(mag1,"testing1.tif")

  #Clean top of image

  print("")

  ## Identify Peaks +++++
  print("===== Identify Peaks =====")
  mag2[0:100,] <- mean(mag2)
  mag2[(nrow(mag2)-60):(nrow(mag2)),] <- mean(mag2)
  mag2[mag2 < 0] <- 0


  col_sums <- colSums(mag2)
  row_sums <- rowSums(mag2)


  #Finding Peaks

  plot(row_sums, type="l", col="navy")
  Peaks <- findpeaks(row_sums, npeaks=4, threshold=(0.8*mean(row_sums)), sortstr=FALSE)
  points(Peaks[, 2], Peaks[, 1], pch=20, col="maroon")
  length(Peaks)
  first_peak_start <- Peaks[1,3]
  first_peak_end <- Peaks[1,4]
  second_peak_start <- try(Peaks[2,3])
  second_peak_end <- try(Peaks[2,4])
  if("try-error" %in% class(second_peak_start)){next}

  if(length(Peaks <= 8)){
    # Have to find both points, lots of fun
    third_peak_start <- first_peak_start
    third_peak_end <- first_peak_end
    fourth_peak_start <- second_peak_start
    fourth_peak_end <- second_peak_end

    # For now, just guess.....
    first_peak_start <- 700
    first_peak_end <- 800

    second_peak_start <- 950
    second_peak_end <- 1050

  }else{
    row_sums[first_peak_start]
    if(length(Peaks) < 16){
      #Only 3 peaks found, need to check which of the two traces is missing
      #If first peak is missing, it should be close to  peak 2 - (peak4 - peak3), which will have different names
      #Check first
      if(row_sums[Peaks[1,2] - (Peaks[3,2]-Peaks[2,2])] > row_sums[0.5*Peaks[1,2]]){
        first_peak_start_old <- first_peak_start
        first_peak_end_old <- first_peak_end
        second_peak_start_old <- second_peak_start
        second_peak_end_old <- second_peak_end
        third_peak_start_old <- third_peak_start
        third_peak_end_old <- third_peak_end

        first_peak_start <- Peaks[1,2] - (Peaks[3,2]-Peaks[2,2]) - abs(Peaks[1,2]-Peaks[1,3])
        first_peak_end <- Peaks[1,2] - (Peaks[3,2]-Peaks[2,2]) + abs(Peaks[1,2] - Peaks[1,4])
        second_peak_start <- first_peak_start_old
        second_peak_end <- first_peak_end_old
        third_peak_start <- second_peak_start_old
        third_peak_end <- second_peak_end_old
        fourth_peak_start <- third_peak_start_old
        fourth_peak_end <- third_peak_end_old
      }
      #Check for second
      if(row_sums[Peaks[1,2] + (Peaks[3,2]-Peaks[2,2])] > row_sums[0.5*Peaks[1,2]]){
        first_peak_start_old <- first_peak_start
        first_peak_end_old <- first_peak_end
        second_peak_start_old <- second_peak_start
        second_peak_end_old <- second_peak_end
        third_peak_start_old <- third_peak_start
        third_peak_end_old <- third_peak_end

        first_peak_start <- first_peak_start_old
        first_peak_end <- first_peak_end_old
        second_peak_start <- Peaks[1,2] + (Peaks[3,2]-Peaks[2,2]) - abs(Peaks[1,2]-Peaks[1,3])
        second_peak_end <- Peaks[1,2] + (Peaks[3,2]-Peaks[2,2]) + abs(Peaks[1,2] - Peaks[1,4])
        third_peak_start <- second_peak_start_old
        third_peak_end <- second_peak_end_old
        fourth_peak_start <- third_peak_start_old
        fourth_peak_end <- third_peak_end_old
      }
      Peaks[1,2] + Peaks[3,2]-Peaks[2,2] - (Peaks[1,2]-Peaks[1,3])
    }else{
      third_peak_start <- Peaks[3,3]
      third_peak_end <- Peaks[3,4]
      fourth_peak_start <- Peaks[4,3]
      fourth_peak_end <- Peaks[4,4]
    }
  }

  #Calculate area between main peaks

  #plot(row_sums[first_peak_start:second_peak_end], type="l", col="navy")

  # Findlines ==========================================================
  # Upper
  startrow <- first_peak_start
  upperbound <- vector(length=ncol(mag1))

  for(c in 1:ncol(mag1)){
    if(mag1[startrow,c]==0 | is.na(mag1[startrow,c])){
      upperbound[c] <- startrow
      next
    }else{
      #Search above and below
      for(i in 1:10){
        above <- startrow + i
        below <- startrow - i
        if(mag1[above,c]==0 | is.na(mag1[above,c])){
          upperbound[c] <- above
          startrow <- above
          break
        }
        if(mag1[below,c]==0|is.na(mag1[below,c])){
          upperbound[c] <- below
          startrow <- below
          break
        }}

    }
  }
  upperbound <- abs(upperbound - nrow(mag1))
  plot(upperbound)

  #Middle
  startrow <- (round((second_peak_start - first_peak_end)/2)+first_peak_end) # This looks strange because the mag2 has the reverse scale
  middle <- c(rep(NA, ncol(mag1)))
  flag11 <- FALSE
  flag_left <- FALSE
  for(c in 1:ncol(mag1)){
    if(mag1[startrow,c]==0 | is.na(mag1[startrow,c])){
      middle[c] <- startrow
      next
    }else{
      for(i in 1:10){
        above <- startrow + i
        below <- startrow - i
        if(mag1[above,c]==0 | is.na(mag1[above,c])){
          middle[c] <- above
          startrow <- above
          next
        }
        if(mag1[below,c]==0 | is.na(mag1[below,c])){
          middle[c] <- below
          startrow <- below
          next
        }
        if(mag1[above,c]==1 & mag1[below,c]==1 & i == 10){
          print("Interection point from left-handside found, starting from right")
          flag11 <- "intersection"
          break
        }}
    }
    if(flag11 == "intersection"){
      left_point <- c(startrow,c)
      if(left_point[1] > round((first_peak_end - second_peak_start)/2) + first_peak_end ){
        flag_left <- "down"
      }else{
        flag_left <- "up"
      }
      break
    }
  }
  #Trace back, if conditions are met, This is for one intersection point
  try(if(flag11 == "intersection"){
    startrow <- round((second_peak_start - first_peak_end)/2) + first_peak_end
    flag11 <- FALSE
    flag_right <- FALSE
    for(c in 1:ncol(mag1)){
      c <- ncol(mag1)-c
      if(mag1[startrow,c]==0 | is.na(mag1[startrow,c])){
        middle[c] <- startrow
        next
      }else{
        for(i in 1:10){
          above <- startrow + i
          below <- startrow - i
          if(mag1[above,c]==0 | is.na(mag1[above,c])){
            middle[c] <- above
            startrow <- above
            next
          }
          if(mag1[below,c]==0 | is.na(mag1[below,c])){
            middle[c] <- below
            startrow <- below
            next
          }
          if(mag1[above,c]==1 & mag1[below,c]==1 & i == 10){
            print("Interection point from right-handside found, determining how many intersections (future work)")
            flag11 <- "intersection"
            break
          }}
      }
      if(flag11 == "intersection"){
        right_point <- c(startrow,c)
        if(right_point[1] > round((first_peak_end - second_peak_start)/2) + first_peak_end ){
          flag_right <- "down"
        }else{
          flag_right <- "up"
        }
        break
      }
    }
  }else{
    flag11 <- FALSE
  })
  # Now we dig!!!!!!!!!
  if(flag11 == "intersection" & flag_left == "down"){
    if(right_point[2]-left_point[2] <= 60){
      middle[left_point[2]:right_point[2]] <- left_point[1]

    }else{
      #Dig from left
      for(i in 1:100){
        below <- startrow - i
        if(mag1[below,left_point[2]]==1){
          next
        }else{
          startrow <- below
          break
        }
      }
      for(c in (left_point[2]+1):(right_point[2]-1)){
        if(mag1[startrow,c]==0){
          middle[c] <- startrow
          next
        }else{
          #Search above and below
          for(i in 1:10){
            above <- startrow + i
            below <- startrow - i
            if(mag1[above,c]==0){
              upperbound[c] <- above
              startrow <- above
              break
            }
            if(mag1[below,c]==0){
              upperbound[c] <- below
              startrow <- below
              break
            }}

        }

      }
    }
  }


  middle <- abs(middle - nrow(mag1))

  #Lower
  startrow <- second_peak_end
  lower <- vector(length=ncol(mag1))
  for(c in 1:ncol(mag1)){
    if(mag1[startrow,c]==0 | is.na(mag1[startrow,c])){
      lower[c] <- startrow
      next
    }else{
      #Search above and below
      for(i in 1:10){
        above <- startrow + i
        below <- startrow - i
        if(mag1[above,c]==0 | is.na(mag1[above,c])){
          lower[c] <- above
          startrow <- above
          break
        }
        if(mag1[below,c]==0 | is.na(mag1[below,c])){
          lower[c] <- below
          startrow <- below
          break
        }}

    }
  }
  lower <- abs(lower - nrow(mag1))

  #Check to varify upperbound, and lowerbound does not start in the wrong place. Can be done by checking the mean of the first few elements

  if(upperbound[1] > mean(upperbound[1:1000])){
    print("line has gone under")
    startrow <- first_peak_start - 60
    upperbound <- vector(length=ncol(mag1))
    for(c in 1:ncol(mag1)){
      if(mag1[startrow,c]==0 | is.na(mag1[startrow,c])){
        upperbound[c] <- startrow
        next
      }else{
        #Search above and below
        for(i in 1:10){
          above <- startrow + i
          below <- startrow - i
          if(mag1[above,c]==0 | is.na(mag1[above,c])){
            upperbound[c] <- above
            startrow <- above
            break
          }
          if(mag1[below,c]==0 | is.na(mag1[below,c])){
            upperbound[c] <- below
            startrow <- below
            break
          }}

      }
    }
    upperbound <- abs(upperbound - nrow(mag1))

  }

  if(lower[1] < mean(lower[1:1000])){
    print("line has gone under")
    startrow <- second_peak_end + 60
    lower <- vector(length=ncol(mag1))
    for(c in 1:ncol(mag1)){
      if(mag1[startrow,c]==0 | is.na(mag1[startrow,c])){
        lower[c] <- startrow
        next
      }else{
        #Search above and below
        for(i in 1:10){
          above <- startrow + i
          below <- startrow - i
          if(mag1[above,c]==0 | is.na(mag1[above,c])){
            lower[c] <- above
            startrow <- above
            break
          }
          if(mag1[below,c]==0 | is.na(mag1[below,c])){
            lower[c] <- below
            startrow <- below
            break
          }}

      }
    }
    lower <- abs(lower - nrow(mag1))

  }

  # Finally, check to varify that the upper and lower bounds have not bounced down/up into the middle section

  if(upperbound[1] > mean(upperbound)){
    print("line has gone under")
    startrow <- first_peak_start - 60
    upperbound <- vector(length=ncol(mag1))
    for(c in 1:ncol(mag1)){
      if(mag1[startrow,c]==0 | is.na(mag1[startrow,c])){
        upperbound[c] <- startrow
        next
      }else{
        #Search above and below
        for(i in 1:10){
          above <- startrow + i
          below <- startrow - i
          if(mag1[above,c]==0 | is.na(mag1[above,c])){
            upperbound[c] <- above
            startrow <- above
            break
          }
          if(mag1[below,c]==0 | is.na(mag1[below,c])){
            upperbound[c] <- below
            startrow <- below
            break
          }}

      }
    }
    upperbound <- abs(upperbound - nrow(mag1))

  }

  if(lower[1] < mean(lower)){
    print("line has gone under")
    startrow <- second_peak_end + 60
    lower <- vector(length=ncol(mag1))
    for(c in 1:ncol(mag1)){
      if(mag1[startrow,c]==0 | is.na(mag1[startrow,c])){
        lower[c] <- startrow
        next
      }else{
        #Search above and below
        for(i in 1:10){
          above <- startrow + i
          below <- startrow - i
          if(mag1[above,c]==0 | is.na(mag1[above,c])){
            lower[c] <- above
            startrow <- above
            break
          }
          if(mag1[below,c]==0 | is.na(mag1[below,c])){
            lower[c] <- below
            startrow <- below
            break
          }}

      }
    }
    lower <- abs(lower - nrow(mag1))

  }

  # Visual on the bounds

  plot(upperbound, type = "l", col = "red", xlim = c(0,6000), ylim = c(0,1800))
  lines(middle, col = "blue")
  lines(lower, col = "purple")



  # Separate Traces ===================================================

  Trace_1sp <- matrix(0,nrow=nrow(mag1),ncol = ncol(mag1))
  Trace_2sp <- matrix(0,nrow=nrow(mag1),ncol = ncol(mag1))

  try(for(c in 1:ncol(mag1)){
    Trace_1sp[(nrow(mag1)-lower[c]):(nrow(mag1)-middle[c]),c] <- mag1[(nrow(mag1)-lower[c]):(nrow(mag1)-middle[c]),c]
    Trace_2sp[(nrow(mag1)-middle[c]):(nrow(mag1)-upperbound[c]),c] <- mag1[(nrow(mag1)-middle[c]):(nrow(mag1)-upperbound[c]),c]
  })


  for(i in 1:ncol(mag1)){
    for(j in 1:nrow(mag1)){
      if(Trace_1sp[j,i]==1){
        Trace_1sp[j,i] <- nrow(mag1) - j
      }
      if(Trace_2sp[j,i]==1){
        Trace_2sp[j,i] <- nrow(mag1) - j
      }
    }
  }
  #writeTIFF(Trace_1sp,"Argh.tif")
  Trace_1sp[Trace_1sp==0] <- NA
  Trace_2sp[Trace_2sp==0] <- NA


  # Find Baselines
  baseline_1 <- mag1[fourth_peak_start:fourth_peak_end,]  #From the bottom
  baseline_2 <- mag1[third_peak_start:third_peak_end,]    #From the bottom

  # writeTIFF(baseline_1,"baseline_1.tif")
  # writeTIFF(baseline_2,"baseline_2.tif")

  # Now we only care about the y-value, not the greyscale
  f_1 <- length(row_sums)-fourth_peak_end
  for(i in 1:ncol(baseline_1)){
    for(j in 1:nrow(baseline_1)){
      if(baseline_1[j,i] > 0){
        baseline_1[j,i] <- j + f_1
      }else{
        baseline_1[j,i] <- NA
      }
    }
  }
  f_2 <- nrow(mag1)-third_peak_end
  for(i in 1:ncol(baseline_2)){
    for(j in 1:nrow(baseline_2)){
      if(baseline_2[j,i] > 0){
        baseline_2[j,i] <- j + f_2
      }else{
        baseline_2[j,i] <- NA
      }
    }
  }

  col_meanb1 <- vector(length= ncol(baseline_1))
  col_meanb2 <- vector(length= ncol(baseline_2))
  for(i in 1:ncol(baseline_1)){
    col_meanb1[i] <- mean(baseline_1[,i],na.rm = TRUE)
  }
  for(i in 1:ncol(baseline_2)){
    col_meanb2[i] <- mean(baseline_2[,i],na.rm = TRUE)
  }

  # plot(col_meanb1, type = "l", col="navy",ylim = c(1,1910))
  # lines(col_meanb2, type = "l", col="red")


  ## Test Plotting
  # trace_1 <- mag1[second_peak_start:second_peak_end,]  #From the bottom, first is at the top of the image
  # trace_2 <- mag1[first_peak_start:first_peak_end,]    #From the bottom

  # writeTIFF(trace_1,"trace_1.tif")
  # writeTIFF(trace_2,"trace_2.tif")

  # t_1 <- nrow(trace_1)
  # f_3 <- length(row_sums)-second_peak_end
  # for(i in 1:ncol(trace_1)){
  #   for(j in 1:t_1){
  #     if(trace_1[j,i] > 0){
  #       trace_1[j,i] <- (t_1 - j) + f_3
  #     }else{
  #       trace_1[j,i] <- NA
  #     }
  #   }
  # }
  # t_2 <- nrow(trace_2)
  # f_4 <- length(row_sums)-first_peak_end
  # for(i in 1:ncol(trace_2)){
  #   for(j in 1:t_2){
  #     if(trace_2[j,i] > 0){
  #       trace_2[j,i] <- (t_2-j) + f_4
  #     }else{
  #       trace_2[j,i] <- NA
  #     }
  #   }
  # }


  plot(colMeans(Trace_2sp,na.rm = TRUE), type = "l",ylim = c(1,1900),xlim = c(1,6000),col="red")
  lines(colMeans(Trace_1sp, na.rm = TRUE),col="blue")
  lines(lower)
  lines(upperbound)
  lines(middle,col="black")
  lines(col_meanb1, col="blue")
  lines(col_meanb2, col ="red")


  #Prepare Data =========================================================================

  Data_Length <- c(1:length(col_sums))
  Trace1 <- colMedians(Trace_1sp,na.rm = TRUE)
  Trace2 <- colMedians(Trace_2sp,na.rm = TRUE)
  Baseline1 <- col_meanb1
  Baseline2 <- col_meanb2

  #Remove Numbers at starts
  tally <- 0
  for(i in 1:length(Data_Length)){
    if(is.na(Baseline1[i])==TRUE){
      tally <- 0
      next
    }else{
      tally <- tally + 1
    }
    if(tally == 60){
      start_of_baseline <- i - 60
      Baseline1[1:start_of_baseline] <- NA
      Trace1[1:start_of_baseline] <- NA
      baseline_start_trace1 <- start_of_baseline
      break
    }
  }
  tally <- 0
  for(i in 1:length(Data_Length)){
    if(is.na(Baseline2[i])==TRUE){
      tally <- 0
      next
    }else{
      tally <- tally + 1
    }
    if(tally == 60){
      start_of_baseline <- i - 60
      Baseline2[1:start_of_baseline] <- NA
      Trace2[1:start_of_baseline] <- NA
      baseline_start_trace2 <- start_of_baseline
      break
    }
  }

  #Remove Numbers at ends
  print("Removing Letter/Numbers")
  tally <- 0
  for(i in 1:length(Data_Length)){
    i <- length(Data_Length) + 1 - i
    if(is.na(Baseline1[i])==TRUE){
      tally <- 0
      next
    }else{
      tally <- tally + 1
    }
    if(tally == 60){
      start_of_baseline <- i + 60
      Baseline1[start_of_baseline:length(Data_Length)] <- NA
      Trace1[start_of_baseline:length(Data_Length)] <- NA
      baseline_end_trace1 <- start_of_baseline
      break
    }
  }

  tally <- 0
  for(i in 1:length(Data_Length)){
    i <- length(Data_Length) + 1 - i
    if(is.na(Baseline2[i])==TRUE){
      tally <- 0
      next
    }else{
      tally <- tally + 1
    }
    if(tally == 60){
      start_of_baseline <- i + 60
      Baseline2[start_of_baseline:length(Data_Length)] <- NA
      Trace2[start_of_baseline:length(Data_Length)] <- NA
      baseline_end_trace2 <- start_of_baseline
      break
    }
  }

  # Fill gaps
  print("Filling large gaps")

  tally <- 0
  jj <- 0
  flag111 <- FALSE
  try(for(i in 1:length(Trace1)){
    if(is.na(Trace1[i])==FALSE){
      flag111 <- TRUE
    }
    if(flag111 == TRUE & is.na(Trace1[i])==TRUE){
      tally <- tally +1
    }
    if(tally >= 10 & is.na(Trace1[i])==FALSE){
      gap_end <- Trace1[i]
      gap_start <- Trace1[i-1-tally]
      if(gap_start > gap_end){
        for(j in (i-tally):(i-1)){
          jj <- jj + 1
          Trace1[j] <- -((gap_start - gap_end)/(i - (i-1-tally)))*jj + gap_start
        }
      }else{
        if(gap_start < gap_end){
          for(j in (i-tally):(i-1)){
            jj <- jj + 1
            Trace1[j] <- ((gap_end-gap_start)/(i - (i-1-tally)))*jj + gap_start
          }
        }else{
          if(gap_start == gap_end){
            for(j in (i-tally):(i-1)){

              Trace1[j] <- gap_start
            }
          }}
      }
      tally <- 0
      jj <- 0
    }
  })

  tally <- 0
  jj <- 0
  flag111 <- FALSE
  try(for(i in 1:length(Trace2)){
    if(is.na(Trace2[i])==FALSE){
      flag111 <- TRUE
    }
    if(flag111 == TRUE & is.na(Trace2[i])==TRUE){
      tally <- tally +1
    }
    if(tally >= 10 & is.na(Trace2[i])==FALSE){
      gap_end <- Trace2[i]
      gap_start <- Trace2[i-1-tally]

      if(gap_start > gap_end){
        for(j in (i-tally):(i-1)){
          jj <- jj + 1
          Trace2[j] <- -((gap_start - gap_end)/(i - (i-1-tally)))*jj + gap_start
        }
      }else{
        if(gap_start < gap_end){
          for(j in (i-tally):(i-1)){
            jj <- jj + 1
            Trace2[j] <- ((gap_end-gap_start)/(i - (i-1-tally)))*jj + gap_start
          }
        }else{
          if(gap_start == gap_end){
            for(j in (i-tally):(i-1)){

              Trace2[j] <- gap_start
            }
          }
        }
        tally <- 0
        jj <- 0
      }
    }
  }
  )



  Data_Out <- cbind(Data_Length,Trace1,Trace2,Baseline1,Baseline2)
  plot(x = Data_Out[,1],y = Data_Out[,3],type = "l", ylim = c(0,1839), xlim = c(0,6240))
  lines(Data_Out[,2], type = "l", col="black")
  lines(Data_Out[,4], type = "l", col="black")
  lines(Data_Out[,3], type = "l", col="black")
  lines(Data_Out[,5], type = "l", col="black")
  lines(lower,col = "purple")
  lines(upperbound, col = "red")
  lines(middle,col="blue")
  data_out1 <- Data_Out

  # Check for gaps in traces, time gaps for a given image are very close in length, this needs to be done per folder. For these images the gaps are 15.


  filename <- file.names[O]

  name_of_file <- print(paste0("Digitized_Data/Algorithm2/",filename,".csv"))

  write.csv(Data_Out,file = name_of_file, row.names = FALSE)

}




#Stiching the image together
day2 <- Data_Out[,3]
day2 <- day2[is.na(day2)==FALSE]
day1 <- Data_Out[,2]
day1 <- day1[is.na(day1)==FALSE]

day1_last <- day1[length(day1)]
day2_start <- day2[1]

new_day2 <- day2 - (day2_start - day1_last)

new_vector <- c(day1,new_day2)
plot(new_vector,type = "l", ylim = c(0,1839), xlim = c(0,12000))

