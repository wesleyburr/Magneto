library("tiff")
library("png")
library("data.table")
PWD <- setwd("~/Magneto2020/ImagesPNG/")

DigitizationTODO <- read.csv("~/Magneto2020/DataCSV/pngTODO200620.csv", header = FALSE, stringsAsFactors = FALSE)
names(DigitizationTODO) <- c("ImagePath", "ImageName", "ConvertedYet", "DigitizationPath", "DigitizationName", "ErrorWhenDigitized")


## Functions -------------------------------------------------------------------
image_import <- function(image,file_loc){
  readTIFF(paste0(file_loc,"/",image))
}
imageChecking <- function(imageName,locationToCheck){
  splitMag <- strsplit(imageName, "-")
  firstThing <- splitMag[[1]][locationToCheck]
  return(firstThing)
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

## Main ------------------------------------------------------------------------


counter = 10799
wrongName <- vector()

for (i in 10800:length(DigitizationTODO$ImageName)) {
  imageName <-  as.character(DigitizationTODO$ImageName[i])
  imagePath <- DigitizationTODO$ImagePath[i]
  ErrorMessage <- DigitizationTODO$ErrorWhenDigitized[i]
  IsPng <- DigitizationTODO$ConvertedYet[i]
  firstPartOfName <- imageChecking(imageName, 1)
  tiffBool <- isTiff(imageName)


  if (IsPng == FALSE){
    if (as.numeric(file.info(paste0(imagePath,imageName))[1]) != 0) { #if there is a size(takes out the ones with 0Bites)
      if (is.na(ErrorMessage)) { # if there is an error message about the data (like there isn't any data)
        if (firstPartOfName == "AGC" || firstPartOfName == "TOR") {
          if (tiffBool == TRUE) { # means that it is a .tiff file
            counter = counter + 1
            print(counter)


            mag <- image_import(imageName, imagePath)
            writePNG(mag, target = paste0(imageName,".png"))
            DigitizationTODO$ConvertedYet[i] = TRUE
            #write.csv(DigitizationTODO, file = "~/Magneto2020/DataCSV/pngTODOTesting.csv")

          }
          else {
            print("not a TIFF")
            print(imagePath)
            print(imageName)
            wrongName <- c(wrongName, paste0(imagePath, imageName))
          }
        }
        else{
          print("not AGC or TOR")
          print(imagePath)
          print(imageName)
          wrongName <- c(wrongName, paste0(imagePath, imageName))
        }
      }
      else{
        print("existing error")
        print(imagePath)
        print(imageName)
        wrongName <- c(wrongName, paste0(imagePath, imageName))
      }
    }
    else{
      print("No Data")
      print(imagePath)
      print(imageName)
      wrongName <- c(wrongName, paste0(imagePath, imageName))
    }
  }
  else{
    print(paste0(imageName," has been converted"))

  }
}
