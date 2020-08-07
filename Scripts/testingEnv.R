library("magneto")
ImageTesting <- read.csv("~/Magneto2020/DataCSV/TODOBatch5.csv", header = TRUE, stringsAsFactors = FALSE)
# k <- 1
# j <- sample(1:26000, size = 100, replace = FALSE)
#
#
# for (k in 1:100) {
#
#   i <- j[k]
#   file_loc <- as.character(ImageTesting[i,1])
#   imageName <- as.character(ImageTesting[i,2])
#
#   imageRAW <- tiff_import(fileName = imageName, fileLoc = file_loc)
#   image <- magneto::.horizontal_image_check(imageRAW)
#   imagecut <- magneto::.trim_top_bottom(image, trimAmountTop = 100, trimAmountBottom = 50)
#   imageSides <- magneto::.get_trace_start_ends(imagecut, returnMat = FALSE, cutPercentage = 2)
#   imageProcessed <- magneto::.process_image(imagecut, cutoffQuantile = 0.90, cutoffProbability = 0.4)
#   imageMatrix <- imageProcessed$gaussImageMatrix
#   imageWithoutSides <- imageMatrix[, -c(0:imageSides$Start, imageSides$End:ncol(imageMatrix))]
#   topcut <- magneto::.top_image_cut(imageMatrix = imageWithoutSides, percentFromEdge = 2, percentEdgeForLeft = 25)
#   bottomcut <- tryCatch(magneto::.bottom_image_cut(imageMatrix = imageWithoutSides, percentEdgeForLeft = 25,
#                                                    percentFromEdge = 2, shortestAlowedSeqOfZeros = 15), warning = function(w) w)
#   if (inherits(bottomcut, "warning")) {
#     print(bottomcut)
#     print(imageName)
#     bottomcut <- ncol(imageMatrix) # this is the only way I know of doing this
#   }
#   imageMatrixTopCut <- imageMatrix[-c(0:topcut),]
#
#
#
#
#
#
#   # Open a pdf file
#   png(paste0("~/Magneto2020/plottingTesting/", imageName, ".png"))
#   # 2. Create a plot
#   suppressWarnings(plot(imageMatrix))
#   abline(h = (nrow(imageMatrix) - topcut), col = "green")
#   abline(h = (nrow(imageMatrix) - bottomcut), col = "green")
#   abline(v = c(imageSides$Start, imageSides$End), col = "green")
#   text(imageSides$Start, x = 800, y = 1500, col = "green")
#   text(imageSides$End, x = 5000, y = 1500, col = "green")
#   # Close the pdf file
#   dev.off()
# }



max_roc <- 10

min_white <- apply(rolledImage, MARGIN = 2, FUN = function(x) {
  if (sum(x) == 0) {
    white <- 0
  }
  else {
    white <- min( which(x == 1) )
  }
  return(white)
})
#starting at the middle to be safe, will work backwords after
foundNonZero <- FALSE
for (i in (round(ncol(rolledImage))/2):ncol(rolledImage)) { # need for loop because need to be able to just back an index
  x <- min_white[i]
  #first column or no nonZero column found yet
  if ( i == 1 || isFALSE(foundNonZero)) {
    if (min_white[i] != 0) {
      foundNonZero <- TRUE
    }
  }
  # a non zero column is found
  else if (isTRUE(foundNonZero)) {
    oneLess <- min_white[i - 1]
    diff <- x - oneLess
    if (diff >= max_roc) { # big change, could be a jump
      if (diff > 0) { # new point is higher then old point
        min_white[i] <- min_white[i - 1] + 2 # 2 added to be safe
      }
      if (diff < 0) { # new point is below old point, cold be a timing gap
        min_white[i] <- min_white[i - 1] - 2 # 2 subtracted to be safe
      }
    }
  }
}

# The reverse, the first half of the image
foundNonZero <- FALSE
for (j in 1:(round(ncol(rolledImage))/2 )) { # need for loop because need to be able to just back an index
  i <- (round(ncol(rolledImage))/2 + 1) - j
  x <- min_white[i]
  #first column or no nonZero column found yet
  if ( j == 1 || isFALSE(foundNonZero)) {
    if (min_white[i] != 0) {
      foundNonZero <- TRUE
    }
  }
  # a non zero column is found
  else if (isTRUE(foundNonZero)) {
    oneLess <- min_white[i + 1] # actually more because reverse indexing
    diff <-  oneLess - x # remember that the picture is reversed aswell, 0 is the top..
    if (diff >= max_roc) { # big change, could be a jump
      #browser()
      if (diff > 0) { # new point is higher then old point (looking at the image)
        min_white[i] <- min_white[i + 1]
      }
      if (diff < 0) { # new point is below old point, cold be a timing gap (lookug at the image)
        min_white[i] <- min_white[i + 1]
      }
    }
  }
}

correctedWhite <- nrow(imageWithoutTopBottom) - min_white
plot(imageWithoutTopBottom)
lines(y = correctedWhite, x = 1:ncol(imageWithoutTopBottom), col = "green")
