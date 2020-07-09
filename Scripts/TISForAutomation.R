
TISForAutomation <- function(file_location = 0, image_name = FALSE,
                numTraces = 2, withplots = TRUE, optimization = TRUE, saveresults = TRUE) {
#withplots alows the user to look at the three lines, the lower, upper and middle bounds
  library("tiff")
  library("pracma")
  library("multitaper")

  source("~/Magneto2020/Scripts/BreakoutOptimization.R")
  source("~/Magneto2020/Scripts/CustomFunctions.R")
  source("~/Magneto2020/Scripts/Functions.R")

  ## Functions ------------------------------------------------------------------




  classArrayEdit <- function(magnetogram){
    magnetogram <- magnetogram[,,1]
    magnetogram <- 1/magnetogram
    if (min(magnetogram) == 1) {
      magnetogram <- magnetogram - 1
      magnetogram <- magnetogram/max(magnetogram)
    }
    return(magnetogram)
  }

  verticalImageCheck <- function(magnetogram){
    ncol_magnetogram <- ncol(magnetogram)
    nrow_magnetogram <- nrow(magnetogram)

    if (ncol_magnetogram < nrow_magnetogram) {
      retVal <- apply(magnetogram,1,rev)
    }
    else {
      retVal <- magnetogram
    }
    return(retVal)
  }
  brightImages <- function(magnetogram){
    magnetogram <- 1 / magnetogram
    if (min(magnetogram) == 1) {
      magnetogram <- magnetogram - 1
      magnetogram <- magnetogram / max(magnetogram)
    }
    return(magnetogram)
  }


  START_INDEX = 1
  END_INDEX = 0
  browser()



  ## Script --------------------------------------------------------------------


  image <- tryCatch(image_import(image = image_name,file_loc = file_location), error = function(e) e)

  if (inherits(image, "error")) {
    return(image)
    }

  if (class(image) == "array") {

    image <- classArrayEdit(image)

  }
  #removed this because I dont see why its relivent.. TODO
  #if (bright == TRUE) {

   #image <- brightImages(image)
  #}

  image <- verticalImageCheck(image)

## lets us check if the image has the correct contrast
  writeTIFF(image,"testing1_auto.tif")

  probabilityB <- brightProb(image)



  print("===== Preprocessing for image =====")



  ## Identifying the Peaks -----------------------------------------------------------------
  print("")
  print("===== Identify Peaks =====")

  if (bright == FALSE) {

    gaussImage <- abs(t( apply(image, MARGIN = 1, FUN = deconvGauss, sig = 10, kern.trunc = 0.05, nw = 3 ) ))
    #writeTIFF(gaussImage,"testing2_auto.tif")

    image[image = NULL] <- 0
    image[image < (1 - mean(image,na.rm = TRUE))] <- 0
    image[image > 0] <- 1
    #writeTIFF(image,"testing3.tif")


    gaussImage[0:100,] <- mean(gaussImage)
    gaussImage[(nrow(gaussImage) - 60):(nrow(gaussImage)),] <- mean(gaussImage)
    gaussImage[gaussImage < 0] <- 0

    col_sums <- colSums(gaussImage)
    rowSums <- rowSums(gaussImage)^2
    threshold <- (0.8*mean(rowSums))
    fivePercent <- 0.05*max(rowSums) #(sum(gaussImage[(nrow(gaussImage) - 130),]) + 1)^2
    distance <- 100

    source_python("~/Magneto2020/Scripts/findPeaks.py")
    peaks <- FindingPeaks(rowSums, fivePercent, distance)
    peaks[[1]] <- peaks[[1]] + 1
  }
  else {# this is bright = TRUE


    image[image = NULL] <- 0
    image[image < (quantile(image,0.95))] <- 0
    image[image > 0] <- 1
    gaussImage <-  t(apply(image, MARGIN = 1, FUN = deconvGauss, sig = 10, kern.trunc = 0.05, nw = 3 ))
    #writeTIFF(gaussImage,"testing2_auto.tif")

    print("")
    print("===== Identify Peaks With Brightness =====")


    gaussImage[0:100, ] <- mean(gaussImage)
    gaussImage[(nrow(gaussImage)-60):nrow(gaussImage),] <- mean(gaussImage)
    gaussImage[gaussImage < 0] <- 0

    col_sums <- colSums(gaussImage)
    rowSums <- rowSums(gaussImage)^2
    threshold <- (0.4*(mean(rowSums)))
    fivePercent <- 0.05*max(rowSums)
    distance <- 50

    source_python("~/Magneto2020/Scripts/findPeaks.py")
    peaks <- FindingPeaks(rowSums, fivePercent, distance)
    peaks[[1]] <- peaks[[1]] + 1 #changed to correct indexes for R instead of python

  }


  #checking to see if the peak is near the bottom/top of the image becuase this can cause flairs
  Edge_Peaks_Check(peaks, rowSums)
  finalPeaks <- Four_Peaks(peaks)

browser()
Peaks <- findpeaks(rowSums, npeaks = 4, threshold = threshold , sortstr = FALSE, minpeakheight = fivePercent)

print("Identify Peaks is done!")

plot(rowSums, type = "l", col = "navy")
points(x = finalPeaks$PeakIndex, y = finalPeaks$PeakHeight, pch = 20, col = "red")
points(x = Peaks[,2], y = Peaks[,1], pch = 11, col = "blue")
#abline(v = c(Peaks[,3], Peaks[,4]), lty = 2, col = "green")
browser()
length(Peaks)

#TODO there is still problems with this
#Error in if (rowSums[Pracmapeaks[1, 2] - (Pracmapeaks[3, 2] - Pracmapeaks[2,  :
#argument is of length zero
PossiblePeaks <- findingPossiblePeaks(Peaks,rowSums = rowSums)

plot(rowSums, type = "l", col = "navy")
points(x = Peaks[,2], y = Peaks[,1], pch = 20, col = "red")
abline(v = c(PossiblePeaks[1,], PossiblePeaks[2,]), lty = 2, col = "green")

#Calculate area between main peaks


# Find lines ==========================================================
# Upper
print("Finding upperbound...")
startrow <- round(PossiblePeaks$firstPeak[START_INDEX]) - 50
upperbound <- vector(length = ncol(image))
for (c in 1:ncol(image)) {
  if (image[startrow, c] == 0) {
    upperbound[c] <- startrow
    next
  } else{
    #Search above and below
    for (i in 1:10) {
      above <- startrow + i
      below <- startrow - i
      if (image[above, c] == 0) {
        upperbound[c] <- above
        startrow <- above
        break
      }
      if (image[below, c] == 0) {
        upperbound[c] <- below
        startrow <- below
        break
      }
    }

  }
}
upperbound <- abs(upperbound - nrow(image))

#Middle
print("finding middle line...")
startrow <-
  round((PossiblePeaks$firstPeak[END_INDEX] - PossiblePeaks$secondpeak[START_INDEX]) / 2) + PossiblePeaks$firstPeak[END_INDEX]
if (bright == TRUE) {
  startrow <-
    ((second_peak_end - round(PossiblePeaks$firstPeak[START_INDEX])) / 2) + PossiblePeaks$firstPeak[START_INDEX]
}
middle <- c(rep(NA, ncol(image)))
flag11 <- FALSE
flag_left <- FALSE
for (c in 1:ncol(image)) {
  if (image[startrow, c] == 0) {
    middle[c] <- startrow
    next
  } else{
    for (i in 1:10) {
      above <- startrow + i
      below <- startrow - i
      if (image[above, c] == 0) {
        middle[c] <- above
        startrow <- above
        next
      }
      if (image[below, c] == 0) {
        middle[c] <- below
        startrow <- below
        next
      }
      if (image[above, c] == 1 & image[below, c] == 1 & i == 10) {
        print("Intersection point from left-handside found, starting from right")
        flag11 <- "intersection"

        break
      }
    }
  }
  print(c)
  upperbound <- abs(upperbound - nrow(image))
  plot(
    middle,
    type = "l",
    col = "red",
    xlim = c(0, 6000),
    ylim = c(0, 1800)
  )
  lines(upperbound, col = "blue")
  if (flag11 == "intersection") {
    left_point <- c(startrow, c)
    if (left_point[1] > round((PossiblePeaks$firstPeak[END_INDEX] - PossiblePeaks$secondpeak[START_INDEX]) / 2) +
        PossiblePeaks$firstPeak[END_INDEX]) {

      flag_left <- "down"
    } else{
      flag_left <- "up"
    }
    break
  }
}



##End of function
}






















