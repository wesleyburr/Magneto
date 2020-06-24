
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


  image <- image_import(image = image_name,file_loc = file_location)

  print(paste0("The minimum value in image is: ", min(image)))
  print(paste0("The maximum value in image is: ", max(image)))


  if (class(image) == "array") {

    image <- classArrayEdit(image)

  }
  if (bright == TRUE) {

    image <- brightImages(image)
  }



  image <- verticalImageCheck(image)



  ## lets us check if the image has the correct contrast
  writeTIFF(image,"testing1_auto.tif")
  gaussImage <- abs(t( apply(image, MARGIN = 1, FUN = deconvGauss, sig = 10, kern.trunc = 0.05, nw = 3 ) ))
  browser()
  print("")
  writeTIFF(gaussImage,"testing2_auto.tif")

  print("===== Preprocessing for image =====")



  ## Identifying the Peaks -----------------------------------------------------------------
  print("")
  print("===== Identify Peaks =====")

  if (bright == FALSE) {

    image[image < (1 - mean(image,na.rm = TRUE))] <- 0
    image[image > 0] <- 1
    writeTIFF(image,"testing3.tif")

    gaussImage[0:100,] <- mean(gaussImage)
    gaussImage[(nrow(gaussImage) - 60):(nrow(gaussImage)),] <- mean(gaussImage)
    gaussImage[gaussImage < 0] <- 0

    col_sums <- colSums(gaussImage)
    rowSums <- rowSums(gaussImage)
    threshold <- (0.8*mean(rowSums))
    print("Identify Peaks is done!")

  }
  else {# this is bright = TRUE

    image[image < (quantile(image,0.90))] <- 0
    image[image > 0] <- 1

    print("")
    print("===== Identify Peaks With Brightness =====")


    gaussImage[0:100, ] <- mean(gaussImage)
    gaussImage[(nrow(gaussImage) - 60):(nrow(gaussImage)),] <- mean(gaussImage)
    gaussImage[gaussImage < 0] <- 0

    col_sums <- colSums(gaussImage)
    rowSums <- rowSums(gaussImage)
    threshold <- (0.8*mean(rowSums))



  }


Peaks <- findpeaks(rowSums, npeaks = 4, threshold = threshold , sortstr = FALSE)
browser()
print("Identify Peaks is done!")

plot(rowSums, type = "l", col = "navy")
points(x = Peaks[,2], y = Peaks[,1], pch = 20, col = "red")
abline(v = c(Peaks[,3], Peaks[,4]), lty = 2, col = "green")

length(Peaks)

first_peak_start <- Peaks[1,3]
first_peak_end <- Peaks[1,4]

second_peak_start <- try(Peaks[2,3])
second_peak_end <- try(Peaks[2,4])






##End of function
}






















