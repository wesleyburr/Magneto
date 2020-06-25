#need to add these to the documentation
library("tiff")
library("pracma")
library("multitaper")
library("data.table")
library("functional")
library("OpenImageR")


#' Finds all files that contain a specific string, in this case for finding files
#' that contain a specific year.
#' @param path Where you would like the function to look.
#' @param keyword The word that you are trying to find in file names.
#' @return  Vector with all paths for each item found with the keyword contained in it.
#' @export

findPathsForkeyword <- function(path = "~/", keyword = NULL){
  library("fs")
  if (is.null(keyword)) {
    return(stop("Must not have null in the specified argument"))
  }
  word = paste0("*",as.character(keyword),"*")

  spathWithKeyword = vector()
  spath = vector

  symlinks <- as.character(fs::dir_ls(path = path , recurse = TRUE, type = "symlink"))
  if (!identical(symlinks, character(0))) { #The character 0 is what is displayed if no symlinks show up
    symL <- length(symlinks)
    for (i in 1:symL) {
      spath <- c(spath, as.character(fs::dir_ls(path = symlinks[i], glob = word, recurse = TRUE, type = "file", fail = FALSE)))
    }

  }
  pathWithKeyword  <- fs::dir_ls(glob = word, path = path, recurse = TRUE, type = "any")

  allPathWithKeyword <- c(spath, pathWithKeyword)
  return(as.character(allPathWithKeyword[-1]))
}


#' Checks a given file path with name to see if the file is empty 0b
#' @param filePath The path for the directory where the file is located
#' @param fileName The name of the file located in the path directory
#' @return a bool of TRUE or FALSE
#' @export
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


#' Checks to see if the image is vertical or horizontal
#' @param array The array of an imported tiff,png,etc...
#' @return The array horizontally
#' @export
verticalImageCheck <- function(array){
  ncol_array <- ncol(array)
  nrow_array <- nrow(array)

  if (ncol_array < nrow_array) {
    retVal <- apply(array,1,rev)
  }
  else {
    retVal <- array
  }
  return(retVal)
}

#' Uses a data frame user imputed into pracma to calculate the first 4 starts and ends for the peaks
findingPossiblePeaks <- function(Pracmapeaks, rowSums){

  first_peak_start <- Pracmapeaks[1,3]
  first_peak_end <- Pracmapeaks[1,4]
  second_peak_start <- try(Pracmapeaks[2,3])
  second_peak_end <- try(Pracmapeaks[2,4])
  if ("try-error" %in% class(second_peak_start)) {next}


  if (length(Pracmapeaks <= 8)) {
    # Have to find both points, lots of fun
    third_peak_start <- first_peak_start
    third_peak_end <- first_peak_end
    fourth_peak_start <- second_peak_start
    fourth_peak_end <- second_peak_end

    #TODO For now, just guess.....
    first_peak_start <- 700
    first_peak_end <- 800

    second_peak_start <- 950
    second_peak_end <- 1050

  }else {
    rowSums[first_peak_start]
    if (length(Pracmapeaks) < 16) {
      #Only 3 Pracmapeaks found, need to check which of the two traces is missing
      #If first peak is missing, it should be close to  peak 2 - (peak4 - peak3), which will have different names
      #Check first
      if (rowSums[Pracmapeaks[1,2] - (Pracmapeaks[3,2]-Pracmapeaks[2,2])] > rowSums[0.5*Pracmapeaks[1,2]]) {
        first_peak_start_old <- first_peak_start
        first_peak_end_old <- first_peak_end
        second_peak_start_old <- second_peak_start
        second_peak_end_old <- second_peak_end
        third_peak_start_old <- third_peak_start
        third_peak_end_old <- third_peak_end

        first_peak_start <- Pracmapeaks[1,2] - (Pracmapeaks[3,2]-Pracmapeaks[2,2]) - abs(Pracmapeaks[1,2] - Pracmapeaks[1,3])
        first_peak_end <- Pracmapeaks[1,2] - (Pracmapeaks[3,2]-Pracmapeaks[2,2
        ]) + abs(Pracmapeaks[1,2] - Pracmapeaks[1,4])
        second_peak_start <- first_peak_start_old
        second_peak_end <- first_peak_end_old
        third_peak_start <- second_peak_start_old
        third_peak_end <- second_peak_end_old
        fourth_peak_start <- third_peak_start_old
        fourth_peak_end <- third_peak_end_old
      }
      #Check for second
      if(rowSums[Pracmapeaks[1,2] + (Pracmapeaks[3,2]-Pracmapeaks[2,2])] > rowSums[0.5*Pracmapeaks[1,2]]){
        first_peak_start_old <- first_peak_start
        first_peak_end_old <- first_peak_end
        second_peak_start_old <- second_peak_start
        second_peak_end_old <- second_peak_end
        third_peak_start_old <- third_peak_start
        third_peak_end_old <- third_peak_end

        first_peak_start <- first_peak_start_old
        first_peak_end <- first_peak_end_old
        second_peak_start <- Pracmapeaks[1,2] + (Pracmapeaks[3,2]-Pracmapeaks[2,2]) - abs(Pracmapeaks[1,2]-Pracmapeaks[1,3])
        second_peak_end <- Pracmapeaks[1,2] + (Pracmapeaks[3,2]-Pracmapeaks[2,2]) + abs(Pracmapeaks[1,2] - Pracmapeaks[1,4])
        third_peak_start <- second_peak_start_old
        third_peak_end <- second_peak_end_old
        fourth_peak_start <- third_peak_start_old
        fourth_peak_end <- third_peak_end_old
      }
      Pracmapeaks[1,2] + Pracmapeaks[3,2]-Pracmapeaks[2,2] - (Pracmapeaks[1,2]-Pracmapeaks[1,3])
    }else{
      third_peak_start <- Pracmapeaks[3,3]
      third_peak_end <- Pracmapeaks[3,4]
      fourth_peak_start <- Pracmapeaks[4,3]
      fourth_peak_end <- Pracmapeaks[4,4]
    }
  }
  allPeaks <- data.frame(firstPeak = c(first_peak_start, first_peak_end),
                         secondPeak = c(second_peak_start, second_peak_end),
                         thirdPeak = c(third_peak_start, third_peak_end),
                         fourthPeak = c(fourth_peak_start, fourth_peak_end),
                         row.names = c("Start","End"))
                         return(allPeaks)
}
