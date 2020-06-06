TISForAutomation <- function(file_location = 0, image_name = FALSE,
                numTraces = 2, withplots = TRUE, optimization = TRUE, saveresults = TRUE, bright = FALSE) {


  library("tiff")
  library("pracma")
  library("multitaper")

  source("~/Magneto2020/Scripts/BreakoutOptimization.R")
  source("~/Magneto2020/Scripts/CustomFunctions.R")

  ## Fuctions ------------------------------------------------------------------

  image_import <- function(image,file_loc){
    readTIFF(paste0(file_loc,"/",image))
    }

  brightImages <- function(magnetogram){
    magnetogram <- 1 / magnetogram
    if (min(magnetogram) == 1) {
      magnetogram <- magnetogram - 1
      magnetogram <- magnetogram / max(magnetogram)
    }
    return(magnetogram)
  }

  classArrayEdit <- function(magnetogram){
    magnetogram <- magnetogram[,,1]
    magnetogram <- 1/magnetogram
    if (min(magnetogram) == 1) {
      magnetogram <- magnetogram - 1
      magnetogram <- magnetogram/max(magnetogram)
    }
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

  yearChecking <- function(magnetogram){
    browser()
    splitMag <- strsplit(magnetogram, "-")
    yearMonthDay <- splitMag[[1]][3]
    separating <- strsplit(yearMonthDay,"")
    year <- paste(separating[1], separating[2], separating[3], separating[4])
    return(year)
  }

  meanPeaks <- function(magnetogram){
    magnetogram[0:100, ] <- mean(magnetogram)
    magnetogram[(nrow(magnetogram) - 60):(nrow(magnetogram)),] <- mean(magnetogram)
    magnetogram[magnetogram < 0] <- 0

    col_sums <- colSums(magnetogram)
    row_sums <- rowSums(magnetogram)
    threshold <- (0.8*mean(row_sums))
    return(c(col_sums,row_sums,threshold,magnetogram))
  }

  identifyPeaks <- function(magnetogram1, magnetogram2, brightness){

    meanPkData <- meanPeaks(magnetogram = magnetogram2)
    col_sums <- meanPkData[1]
    row_sums <- meanPkData[2]
    threshold <- meanPkData[3]
    magnetogram2 <- meanPkData[4]

    if (bright == TRUE) {

      magnetogram1[magnetogram1 < (quantile(magnetogram1,0.90))] <- 0
      magnetogram1[magnetogram1 > 0] <- 1

      print("")
      print("===== Identify Peaks With Brightness =====")

      meanPkData <- meanPeaks(magnetogram = magnetogram2)
      col_sums <- meanPkData[1]
      row_sums <- meanPkData[2]
      threshold <- meanPkData[3]
      magnetogram2 <- meanPkData[4]


    }
    return(c(col_sums,row_sums,threshold,magnetogram1,magnetogram2))
  }

  ## Script --------------------------------------------------------------------


  mag1 <- image_import(image = image_name,file_loc = file_location)
  print(paste0("The minimum value in image is: ", min(mag1)))
  print(paste0("The maximum value in image is: ",max(mag1)))


  if (class(mag1) == "array") {
    classArrayEdit(mag1)
    }
  else if (bright == TRUE) {

    brightImages(mag1)
    }
    else{

    }


  mag1 <- verticalImageCheck(mag1)

  ## lets us check if the image has the correct contrast
  writeTIFF(mag1,"testing1_auto.tif")

  mag2 <- t( apply( mag1, MARGIN = 1, FUN = deconvGauss, sig = 10, kern.trunc = 0.05, nw = 3 ) )

  print("")
  writeTIFF(mag2,"testing2_auto.tif")

  print("===== Preprocessing for mag1 =====")

  ## Checking the year of the image

  year <- yearChecking(image_name)

  ## Depending on the year, what to do -----------------------------------------------------

  if (as.numeric(year) <= 1900 ) {
    if (bright == FALSE) {
      browser()
      mag1[mag1 < (1 - mean(mag1, na.rm = TRUE))] <- 0
      mag1[mag1 > 0] <- 1
      writeTIFF(mag1,"testing3_auto.tif")
    }
  }
  else{browser()}

  print("")
  print("===== Identify Peaks =====")


  peaksData <- identifyPeaks(mag1, mag2, brightness = bright)
  col_sums <- peaksData[1]
  row_sums <- peaksData[2]
  threshold <- peaksData[3]
  magnetogram1 <- peaksData[4]
  magnetogram2 <- peaksData[5]
}
































