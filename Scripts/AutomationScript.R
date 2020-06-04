DigitizationUsingTIS <- function(ImageDigitizationDFnx6 = NULL, PWD = NULL,
                                 withplots = TRUE, optimization = TRUE, saveresults = TRUE, bright = FALSE){
  library("tiff")
  library("pracma")
  library("data.table")
  library("functional")
  library("OpenImageR")

  ##Input checking --------------------------------------------------------------------
  if (is.null(ImageDigitizationDFnx6) | is.null(PWD)) {
    Error <- "Not all Parameters are filled in, please fill in and try again"
    return(Error)
  }
  if (dim.data.frame(ImageDigitizationDFnx6)[2] != 6) {
    Error <- "Your dataframe has the wrong dimentions, should be 6 columns with 3rd being TRUE or FALSE"
    return(Error)
  }

  ## If all inputs are good, continue


imagePath <- vector()
imageName <- vector()
  ## Breaking down the file locations -------------------------------------------------
  for (i in 1:3){# 1:length(ImageDigitizationDFnx6[,1])) {
    if (ImageDigitizationDFnx6$DigitizedYet[i] == "True") {
      print(paste0(ImageDigitizationDFnx6$ImageName[i], " has been digitized"))

    }
    if (ImageDigitizationDFnx6$DigitizedYet[i] == "False") {

      oneImagePath <- as.character(ImageDigitizationDFnx6$ImagePath[i])
      oneImageName <- as.character(ImageDigitizationDFnx6$ImageName[i])
      source("~/Magneto2020/Scripts/TISForAutomation.R")
      TISForAutomation(file_location = oneImagePath, image_name = oneImageName , withplots = withplots,
                       optimization = optimization, saveresults = saveresults, bright = bright)



    }
  }

  ## TIS For all different paths -------------------------------------------------------

  for (i in 1:length(imagePath)) {
    oneImagePath <- imagePath[i]
    oneImageName <- imageName[i]
      #Trace Identification through separation

    }


}














