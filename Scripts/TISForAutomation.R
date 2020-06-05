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

}
