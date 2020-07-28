ImageTesting <- read.csv("~/Magneto2020/DataCSV/TODOBatch5.csv", header = TRUE, stringsAsFactors = FALSE)
set.seed(101)
random100 <- sample(1:26000, size = 100, replace = FALSE)
for (i in random100) {
  file_loc <- as.character(ImageTesting[i,1])
  imageName <- as.character(ImageTesting[i,2])

  image <- tiff_import(fileName = imageName, fileLoc = file_loc)
  hImage <- as.matrix(.horizontal_image_check(image))
  bright <- bright(hImage)
  # if (bright == TRUE) {
  processedImage <- .for_bright_image(hImage)
  # }
  # if (bright == FALSE) {
  #   processedImage <- .not_bright_image(hImage)
  # }

  #rmImage <- processedImage$gaussimageMatrix[,-c(0:twoPerc, (columns - twoPerc):columns)]
  rowSumsImage <- rowSums(processedImage$gaussImageMatrix)
  colSumsImage <- colSums(processedImage$gaussImageMatrix)

  #plot(colSumsImage, type = "l")

  len <- length(colSumsImage)
  twoPerc <- round((0.01*len))
  diffsColSms <- abs(diff(colSumsImage))
  possibleStartDiffs <- which(diffsColSms >= 0.05)
  diffsOfDiffs <- diff(possibleStartDiffs)
  chosenDiffs <- possibleStartDiffs[which(abs(diff(possibleStartDiffs))  <= 20)]
  chosenDiffs <- chosenDiffs[which(chosenDiffs <= (len - twoPerc) & chosenDiffs >= twoPerc)]
  first <- chosenDiffs[1]
  newFirst <- first
  last <- chosenDiffs[length(chosenDiffs)]
  newLast <- last
  compareLeft <- colSumsImage[first]
  compareRight <- colSumsImage[last]
  middleMean <- mean(colSumsImage[2000:4000])
  for (j in 1:1000) {
    if (colSumsImage[first + j] <= compareLeft + 1 & colSumsImage[first + j]  <= middleMean - 3) {
      #compare <- colSumsImage[first + j]
      newFirst <- first + j
    }
  }
  for (k in 1:1000) {
    if (colSumsImage[last - k] <= compareRight + 1 & colSumsImage[last - k]  <= middleMean - 3) {
      newLast <- last - k
    }
  }

  plot(colSumsImage, type = "l")
  abline(v = c(newFirst, newLast))
  ImageNoSides <- processedImage$gaussImageMatrix[,-c(0:newFirst, newLast:ncol(hImage))]
  browser()
}
