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


  workingImage <- image_import(image = image_name,file_loc = file_location)
  print(paste0("The minimum value in image is: ", min(workingImage)))
  print(paste0("The maximum value in image is: ",max(workingImage)))


  if (class(workingImage) == "array") {
    classArrayEdit(workingImage)
    }
  else if (bright == TRUE) {

    brightImages(workingImage)
    }
    else{

    }


  workingImage <- verticalImageCheck(workingImage)

  writeTIFF(workingImage,"testing1_auto.tif")


}
