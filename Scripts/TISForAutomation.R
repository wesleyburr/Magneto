TISForAutomation <- function(file_location = 0, image_name = FALSE,
                numTraces = 2, withplots = TRUE, optimization = TRUE, saveresults = TRUE, bright = FALSE) {


  library("tiff")
  library("pracma")
  library("multitaper")

  source("~/Magneto2020/Scripts/BreakoutOptimization.R")
  source("~/Magneto2020/Scripts/CustomFunctions.R")

  ## Functions ------------------------------------------------------------------

  image_import <- function(image,file_loc){
    readTIFF(paste0(file_loc,"/",image))
    }

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






  ## Script --------------------------------------------------------------------


  mag1 <- image_import(image = image_name,file_loc = file_location)

  print(paste0("The minimum value in image is: ", min(mag1)))
  print(paste0("The maximum value in image is: ", max(mag1)))


  if (class(mag1) == "array") {

    mag1 <- classArrayEdit(mag1)

  }
  if (bright == TRUE) {

    mag1 <- brightImages(mag1)
  }



  mag1 <- verticalImageCheck(mag1)



  ## lets us check if the image has the correct contrast
  writeTIFF(mag1,"testing1_auto.tif")
  mag2 <- t( apply(mag1, MARGIN = 1, FUN = deconvGauss, sig = 10, kern.trunc = 0.05, nw = 3 ) )

  print("")
  writeTIFF(mag2,"testing2_auto.tif")

  print("===== Preprocessing for mag1 =====")



  ## Identifying the Peaks -----------------------------------------------------------------
  print("")
  print("===== Identify Peaks =====")

  if (bright == FALSE) {

    mag1[mag1 < (1 - mean(mag1,na.rm = TRUE))] <- 0
    mag1[mag1 > 0] <- 1
    writeTIFF(mag1,"testing3.tif")

    mag2[0:100,] <- mean(mag2)
    mag2[(nrow(mag2) - 60):(nrow(mag2)),] <- mean(mag2)
    mag2[mag2 < 0] <- 0

    col_sums <- colSums(mag2)
    row_sums <- rowSums(mag2)
    threshold <- (0.8*mean(row_sums))
    print("Identify Peaks is done!")

  }
  else {# this is bright = false

    mag1[mag1 < (quantile(mag1,0.90))] <- 0
    mag1[mag1 > 0] <- 1

    print("")
    print("===== Identify Peaks With Brightness =====")


    mag2[0:100, ] <- mean(mag2)
    mag2[(nrow(mag2) - 60):(nrow(mag2)),] <- mean(mag2)
    mag2[mag2 < 0] <- 0

    col_sums <- colSums(mag2)
    row_sums <- rowSums(mag2)
    threshold <- (0.8*mean(row_sums))


  print("Identify Peaks is done!")
  }


#plot(rowSums, type = "l", col = "navy")
#Peaks <- findpeaks(rowSums, npeaks = 4, threshold = threshold , sortstr = FALSE)
#points(Peaks[, 2], Peaks[, 1], pch = 20, col = "maroon")
#length(Peaks)
}
#first_peak_start <- Peaks[1,3]
#first_peak_end <- Peaks[1,4]

#second_peak_start <- try(Peaks[2,3])
#second_peak_end <- try(Peaks[2,4])






























