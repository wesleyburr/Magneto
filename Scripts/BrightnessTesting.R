DigitizationTODO <- read.csv("~/Magneto2020/FinalTable.csv", header = FALSE, stringsAsFactors = FALSE)
names(DigitizationTODO) <- c("ImagePath", "ImageName", "DigitizedYet", "DigitizationPath", "DigitizationName", "ErrorWhenDigitized")

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


i = 1
vec <- vector()
for (i in 1:148) {
  imageName <-  as.character(DigitizationTODO$ImageName[i])
  filePath <- DigitizationTODO$ImagePath[i]
  firstPartOfName <- imageChecking(imageName, 1)
  tiffBool <- isTiff(imageName)

  if (firstPartOfName == "AGC") {
    if (tiffBool == TRUE) { # means that it is a .tiff file
      mag <- image_import(imageName,filePath)
      vec <- c(vec,max(mag))
    }
    else {
      DigitizationTODO$ErrorWhenDigitized[i] <- "Not a tiff"
      DigitizationTODO$DigitizedYet[i] <- "Not tiff"

    }
  }
  else{
    DigitizationTODO$ErrorWhenDigitized[i] <- "Not An Image"


  }
}
