DigitizationUsingTIS <- function(ImageDigitizationDFnx6 = NULL, PWD = NULL,
                                 withplots = TRUE, optimization = TRUE, saveresults = TRUE, bright = FALSE){
  library("tiff")
  library("pracma")
  library("data.table")
  library("functional")
  library("OpenImageR")

  ##Functions -----------------------------------------------------------------------

  imageChecking <- function(imageName){
    splitMag <- strsplit(imageName, "-")
    firstThing <- splitMag[[1]][1]
    return(firstThing)
  }

  yearChecking <- function(magnetogram){

    splitMag <- strsplit(magnetogram, "-")
    yearMonthDay <- splitMag[[1]][4]
    separating <- strsplit(yearMonthDay,"")
    year <- paste0(separating[[1]][1], separating[[1]][2], separating[[1]][3], separating[[1]][4])
    return(year)
  }
  isTiff <- function(imageName){
    splitMag <- strsplit(imageName, "-")
    lastStringInSplit <- strsplit(last(splitMag[[1]]), "")
    lastLetter <- last(lastStringInSplit[[1]])
    if (lastLetter == "f") {
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }

  ##Input checking --------------------------------------------------------------------
  print("im here 1")

  if (is.null(ImageDigitizationDFnx6) | is.null(PWD)) {
    Error <- "Not all Parameters are filled in, please fill in and try again"
    return(Error)
  }
  if (dim.data.frame(ImageDigitizationDFnx6)[2] != 6) {
    Error <- "Your dataframe has the wrong dimentions, should be 6 columns with 3rd being TRUE or FALSE"
    return(Error)
  }

  ## If all inputs are good, continue


  ## Selecting one year ---------------------------------------------------------------


  ## Breaking down the file locations -------------------------------------------------
  print("im here 2")
  for (i in 1:3) {# oneYear)) {
    if (ImageDigitizationDFnx6$DigitizedYet[i] == "True") {
      print(paste0(ImageDigitizationDFnx6$ImageName[i], " has been digitized"))

    }
    if (ImageDigitizationDFnx6$DigitizedYet[i] == "False") {
      oneImagePath <- as.character(ImageDigitizationDFnx6$ImagePath[i])
      oneImageName <- as.character(ImageDigitizationDFnx6$ImageName[i])

      firstPartOfName <- imageChecking(oneImageName)
      tiffBool <- isTiff(oneImageName)

      if (firstPartOfName == "AGC") {
        if (tiffBool == TRUE) { # means that it is a .tiff file
          browser()
          source("~/Magneto2020/Scripts/TISForAutomation.R")
          TISForAutomation(oneImagePath, image_name = oneImageName , withplots = withplots,
                       optimization = optimization, saveresults = saveresults, bright = bright)
        }
        else{
          ImageDigitizationDFnx6$ErrorWhenDigitized[i] <- "Not a tiff"
          ImageDigitizationDFnx6$DigitizedYet[i] <- "Not tiff"
        }
      }
      else{
        ImageDigitizationDFnx6$DigitizedYet[i] == "NotAnImage"
      }
    }

  }

}



















