library("tiff")
library("pracma")
library("data.table")
library("functional")
library("OpenImageR")
library("data.table") # for the last call
source("~/Magneto2020/Scripts/BreakoutOptimization.R") #for gauss


DigitizationTODO <- read.csv("~/Magneto2020/FinalTable.csv", header = FALSE, stringsAsFactors = FALSE)
names(DigitizationTODO) <- c("ImagePath", "ImageName", "DigitizedYet", "DigitizationPath", "DigitizationName", "ErrorWhenDigitized")


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

i = 1
DiffWithAndWithoutGauss <- data.frame(imagePath = NA,  imageName = NA, MatMean = NA, MatMedian = NA,
                                      MatQ1 = NA, MatQ3 = NA, MatMax = NA , MatMin = NA,
                                      GaussMatMean = NA, GaussMatMedian = NA, GaussMatQ1 = NA,
                                      GaussMatQ3 = NA, GaussMatMax = NA, GaussMatMin = NA)
counter = 0
forCounter = 0
wrongName <- vector()
sam <- sample(1:39266, size = 5000 ,replace = FALSE)
for (i in sam) { 
  forCounter <- forCounter + 1
  imageName <-  as.character(DigitizationTODO$ImageName[i])
  imagePath <- DigitizationTODO$ImagePath[i]
  ErrorMessage <- DigitizationTODO$ErrorWhenDigitized[i]
  firstPartOfName <- imageChecking(imageName, 1)
  tiffBool <- isTiff(imageName)

  if (as.numeric(file.info(paste0(imagePath,imageName))[1]) != 0) { #if there is a size(takes out the ones with 0Bites)
    if (is.na(ErrorMessage)) { # if there is an error message about the data (like there isn't any data)
      if (firstPartOfName == "AGC" || firstPartOfName == "TOR") {
        if (tiffBool == TRUE) { # means that it is a .tiff file
          counter = counter + 1
          print(counter)


              mag <- image_import(imageName, imagePath)

              DiffWithAndWithoutGauss[counter, "imagePath"] <- imagePath
              DiffWithAndWithoutGauss[counter, "imageName"] <- imageName
              DiffWithAndWithoutGauss[counter, "MatMean"] <- mean(mag)
              DiffWithAndWithoutGauss[counter, "MatMedian"] <- median(mag)
              DiffWithAndWithoutGauss[counter, "MatMax"] <- max(mag)
              DiffWithAndWithoutGauss[counter, "MatMin"] <- min(mag)
              DiffWithAndWithoutGauss[counter, "MatQ1"] <- as.numeric(quantile(mag, probs = 0.25))
              DiffWithAndWithoutGauss[counter, "MatQ3"] <- as.numeric(quantile(mag, probs = 0.75))


              mag2 <- t( apply( mag, MARGIN = 1, FUN = deconvGauss, sig = 10, kern.trunc = 0.05, nw = 3 ) )

              DiffWithAndWithoutGauss[counter, "GaussMatMean"] <- mean(mag2)
              DiffWithAndWithoutGauss[counter, "GaussMatMedian"] <- median(mag2)
              DiffWithAndWithoutGauss[counter, "GaussMatMax"] <- max(mag2)
              DiffWithAndWithoutGauss[counter, "GaussMatMin"] <- min(mag2)
              DiffWithAndWithoutGauss[counter, "GaussMatQ1"] <- as.numeric(quantile(mag2, probs = 0.25))
              DiffWithAndWithoutGauss[counter, "GaussMatQ3"] <- as.numeric(quantile(mag2, probs = 0.75))

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

write.csv(DiffWithAndWithoutGauss, "DiffWAWOG554IQR.csv")
