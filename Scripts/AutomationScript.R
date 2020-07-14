#'This function breaks down the inputted dataframe and initializes the TISForAutomationFunction
#'@param ImageDigitizedDFnx6 is a user inputed data frame with 6 specific columns. (use DigitizationTODO.py to get the format)
#'@param PWD is the present working directory currently
#'@param keywordInName Certain keyword that the user wants to base the digitization off of
#'@param withPlots Displays plots to the user as the TIS is running
#'@param optimization If user wants the optimization to happen in TIS
#'
#'#TODO need to finish imputing the parameters of the function, return, after we figure out what ones we are including.s
#'

Digitization_Using_TIS <- function(ImageDigitizationDFnx6, PWD, keywordInName = '', TODOcsvName,
                                 withplots = TRUE, optimization = TRUE, saveresults = TRUE){
  library("tiff")
  library("pracma")
  library("data.table")
  library("functional")
  library("OpenImageR")

  ##Input checking --------------------------------------------------------------------
  source("~/Magneto2020/Scripts/Functions/Functions.R")

  if (dim.data.frame(ImageDigitizationDFnx6)[2] != 6) {
    Error <- "Your dataframe has the wrong dimentions, should be 6 columns with 3rd being TRUE or FALSE"
    return(stop(Error))
  }
  ## If all inputs are good, continue


  ## Selecting certain range ---------------------------------------------------------------
  foundWithKeyword <- grep(keywordInName, x = ImageDigitizationDFnx6$ImageName)

  ## Breaking down the file locations -------------------------------------------------


  for (i in foundWithKeyword) {

    if (ImageDigitizationDFnx6$DigitizedYet[i] == "TRUE") {
      print(paste0(ImageDigitizationDFnx6$ImageName[i], " has been digitized"))
    }
    if (ImageDigitizationDFnx6$DigitizedYet[i] == "FALSE") {
      oneImagePath <- as.character(ImageDigitizationDFnx6$ImagePath[i])
      oneImageName <- as.character(ImageDigitizationDFnx6$ImageName[i])

      ErrorMessage <- ImageDigitizationDFnx6$ErrorWhenDigitized[i]



      source("~/Magneto2020/Scripts/Functions/MagSpecificFunctions.R")
      firstPartOfName <- imageChecking(oneImageName)
      source('~/Magneto2020/Scripts/Functions/Functions.R')
      if (notEmptyFile(ImageDigitizationDFnx6$ImagePath[i],ImageDigitizationDFnx6$ImageName[i])) {
        if (is.na(ErrorMessage)) { # if there is an error message about the data (like there isn't any data)
          if (firstPartOfName == "AGC" || firstPartOfName == "TOR")
              source("~/Magneto2020/Scripts/TISForAutomation.R")
              ret <- TISForAutomation(oneImagePath, image_name = oneImageName , withplots = withplots,
                           optimization = optimization, saveresults = saveresults)

              if(inherits(ret, "error")) {
                ImageDigitizationDFnx6$ErrorWhenDigitized[i] <- "Not a tiff"
                ImageDigitizationDFnx6$DigitizedYet[i] <- "Not tiff"
                next
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

  write.csv(ImageDigitizationDFnx6, TODOcsvName)
}





















