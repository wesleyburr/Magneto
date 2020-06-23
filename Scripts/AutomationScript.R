DigitizationUsingTIS <- function(ImageDigitizationDFnx6 = NULL, PWD = NULL, keywordInName = FALSE,
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
  notEmptyFile <- function(filePath = NA, fileName = NA){
    if (is.na(filePath) ||  is.na(fileName)) {
      return(print("missing filePath or fileName"))
    }
    if (as.numeric(file.info(paste0(filePath,fileName))[1]) != 0) {
      return(notEmpty = TRUE)
    }
    else {
      return(notEmpty = FALSE)
    }
  }

  ##Input checking --------------------------------------------------------------------


  if (is.null(ImageDigitizationDFnx6) | is.null(PWD)) {
    Error <- "Not all Parameters are filled in, please fill in and try again"
    return(stop(Error))
  }
  if (dim.data.frame(ImageDigitizationDFnx6)[2] != 6) {
    Error <- "Your dataframe has the wrong dimentions, should be 6 columns with 3rd being TRUE or FALSE"
    return(stop(Error))
  }
  if (is.null(keywordInName)) {
    Error <- "Need to add a specific keyword or a common trait of the file names"
    return(stop(Error))
  }
  browser()
  ## If all inputs are good, continue


  ## Selecting one year ---------------------------------------------------------------
  foundWithKeyword <- grep(keywordInName, x = DigitizationTODO$ImageName)

  ## Breaking down the file locations -------------------------------------------------
#TODO need to get the keywords searched in the file so I know there index

  for (i in foundWithKeyword) {
    if (ImageDigitizationDFnx6$DigitizedYet[i] == "True") {
      print(paste0(ImageDigitizationDFnx6$ImageName[i], " has been digitized"))
    }
    if (ImageDigitizationDFnx6$DigitizedYet[i] == "False") {
      oneImagePath <- as.character(ImageDigitizationDFnx6$ImagePath[i])
      oneImageName <- as.character(ImageDigitizationDFnx6$ImageName[i])

      ErrorMessage <- ImageDigitizationDFnx6$ErrorWhenDigitized[i]
      notEmpty <- notEmptyFile(ImageDigitizationDFnx6$ImagePath[i],ImageDigitizationDFnx6$ImageName[i])
      firstPartOfName <- imageChecking(oneImageName)
      tiffBool <- isTiff(oneImageName)

      if (notEmpty) {
        if (is.na(ErrorMessage)) { # if there is an error message about the data (like there isn't any data)
          if (firstPartOfName == "AGC" || firstPartOfName == "TOR") {
            if (tiffBool) { # means that it is a .tiff file
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
      else{
        ImageDigitizationDFnx6$ErrorWhenDigitized[i] <- "NoDataInFile"
        }
    }
  }
}



















