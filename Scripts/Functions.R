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

#'Using a logistic regression to find coefficients, uses the result to compute a probability
#'of the image being #'bright (392 observations) with McFadden Pseudo- $R^2$ Value 0.2573778
#'@param matrix of image points representing the brightness of that pixel
#'@param cutoffProbability The probability cut off for the desision of an image being bright
#'@return The decision of the image being bright
#'@export
brightProb <- function(image, cutoffProbability = 0.5){
  beta0 <- -2.774327
  beta1 <- 51.91687

  aboveLen <- length(which(image >= 0.80))
  totalLen <- length(image)
  standardizedLen <- aboveLen/totalLen
  decision <- exp(beta0 + beta1 * standardizedLen)/(1 + exp(beta0 + beta1 * standardizedLen))

  if (decision >= cutoffProbability){
    bright = TRUE
  }
  else {
    bright = FALSE
  }
  return(bright)
}

#'checks to see if the last part of a file name is .tif or .tiff *It doesn't split any part of the name*
#'@param imageName in the form AGC--H-19260314-19260316.tif
#'@return bool of TRUE or FALSE
isTiff <- function(imageName){
  lastStringInSplit <- strsplit(imageName, "")
  lenStrSp = length(lastStringInSplit[[1]])
  fileType <- lastStringInSplit[[1]][as.integer(lenStrSp - 3):lenStrSp]
  fileTypeOneString = paste0(fileType[1], fileType[2], fileType[3], fileType[4])
  if (fileTypeOneString == ".tif" | fileTypeOneString == "tiff") {
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}


#'Reads tiff (also checks for if the image is a .tif or .tiff)
#'@param image The image name
#'@param file_location the path from ~/ to the dir where the file is
image_import <- function(image,file_loc){
  tiffCheck <- isTiff(image)

  if(isTRUE(tiffCheck)){
    return(readTIFF(paste0(file_loc,"/",image)))
  }
  else{
    stop(paste0("file ", image, " is not a .tif or .tiff" ))
  }

}

#'Checks the list of peaks for any flairs in the photos at start and end, they are removed
#'@param FindingPeaksVector the resulting vector from FindingPeaks
#'@param rowSums of the data frame
#'@return void
Edge_Peaks_Check <- function(FindingPeaksVector, rowSums){

  for (k in 1:length(FindingPeaksVector[[1]])) {
    if (is.na(rowSums[FindingPeaksVector[[1]][k] + 20])){
      FindingPeaksVector[[1]] <- FindingPeaksVector[[1]][-k]
      FindingPeaksVector[[2]]$peak_heights <- FindingPeaksVector[[2]]$peak_heights[-k]
    }
    else if (is.na(rowSums[FindingPeaksVector[[1]][k] - 20])){
      FindingPeaksVector[[1]] <- FindingPeaksVector[[1]][-k]
      FindingPeaksVector[[2]]$peak_heights <- FindingPeaksVector[[2]]$peak_heights[-k]
    }

    else if (rowSums[FindingPeaksVector[[1]][k] + 20] == rowSums[length(rowSums)]) {

      FindingPeaksVector[[1]] <- FindingPeaksVector[[1]][-k]
      FindingPeaksVector[[2]]$peak_heights <- FindingPeaksVector[[2]]$peak_heights[-k]

    }
    else if (rowSums[FindingPeaksVector[[1]][k] - 20] == rowSums[1]) {

      FindingPeaksVector[[1]] <- FindingPeaksVector[[1]][-k]
      FindingPeaksVector[[2]]$peak_heights <- FindingPeaksVector[[2]]$peak_heights[-k]

    }
  }
  return(FindingPeaksVector)
}



best_Peaks <- function(findingPeaksVector, maxPeaks){

  highestPeaksIndex <- vector()
  bestPeaksHeight <- vector()
  if (length(findingPeaksVector[[1]]) > maxPeaks) {
    sorting <- sort(findingPeaksVector[[2]]$peak_heights, decreasing = TRUE, index.return = TRUE)
    for (j in 1:maxPeaks) {
      highestPeaksIndex <- c(highestPeaksIndex, findingPeaksVector[[1]][sorting$ix[j]])#ix is a product from sort
      bestPeaksHeight <- c(bestPeaksHeight, findingPeaksVector[[2]]$peak_heights[sorting$ix[j]])
    }
  }
  if (length(findingPeaksVector[[1]]) <= maxPeaks) {
    highestPeaksIndex <- findingPeaksVector[[1]]
    bestPeaksHeight <- findingPeaksVector[[2]]$peak_heights
  }
  bestPeaks <- data.frame(highestPeaksIndex, bestPeaksHeight)
  names(bestPeaks) <-  c("PeakIndex", "PeakHeight")
  return(bestPeaks)
}

# TODO put a dimension check on this function
finding_Peak_Start_Ends <- function(peaks){

  peakStart <- vector()
  peakEnd <- vector()
  for (k in 1:length(peaks$PeakIndex)) {
    height <- peaks$PeakHeight[k]
    tempHeightLeft <- height
    tempHeightRight <- height

    for (j in 1:100) {
      leftSide <- peaks$PeakIndex[k] - j
      rightSide <- peaks$PeakIndex[k] + j
      if (rowSums[leftSide] <= tempHeightLeft &
          rowSums[rightSide] <= tempHeightRight) {

        tempHeightLeft <- rowSums[leftSide]
        tempHeightRight <- rowSums[rightSide]
      }
      else if (rowSums[leftSide + 1] <= tempHeightLeft &
               rowSums[rightSide] <= tempHeightRight) {

        tempHeightLeft <- rowSums[leftSide + 1]
        tempHeightRight <- rowSums[rightSide]
      }
      else if(rowSums[leftSide] <= tempHeightLeft &
              rowSums[rightSide + 1] <= tempHeightRight) {

        tempHeightLeft <- rowSums[leftSide]
        tempHeightRight <- rowSums[rightSide + 1]
      }
      else{
        peakStart[k] <- leftSide
        peakEnd[k] <- rightSide
        break
      }

    }


  }
  ret <- data.frame(peaks, peakStart, peakEnd)
  return(ret)
}

find_peaks <- function(rowSums, minDistance, maxPeakNumber){

  library("reticulate")

  fivePercent <- 0.05*max(rowSums)
  source_python("~/Magneto2020/Scripts/findPeaks.py")
  peaks <- FindingPeaks(rowSums, fivePercent, minDistance)
  peaks[[1]] <- peaks[[1]] + 1 # python index correction



  peaksNoEdge <- Edge_Peaks_Check(peaks, rowSums)
  fourPeaks <- best_Peaks(peaksNoEdge, maxPeaks = maxPeakNumber)

  ret <- finding_Peak_Start_Ends(fourPeaks)
  names(ret) <- c("peakIndex", "peakHeight", "peakStarts", "peakEnds")
  return(ret)

}


#' Checks for a null argument
#' @param  parameter what you would like to check if is null
#' @param parameterName Allows the error message to be in context
#' @return stop if is null
#' @export
null <- function(parameter, parameterName){
  if(is.null(parameter)) {
    return(stop(paste0("Need to specify ", parameterName)))
  }

}


