
#'Used to see if the first part of Image name is correct
#'@param imageName Name of an Image
#'@return firstThing the first part of the image name
#'@example AGC-D-19010810-19010811.tif will return AGC
.imageChecking <- function(imageName){
  splitMag <- strsplit(imageName, "-")
  firstThing <- splitMag[[1]][1]
  return(firstThing)
}
