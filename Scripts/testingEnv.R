library("magneto")

ImageTesting <- read.csv("~/Magneto2020/DataCSV/TODOBatch5.csv", header = TRUE, stringsAsFactors = FALSE)
k <- 1
j <- sample(1:26000, size = 100, replace = FALSE)

for (k in 1:100) {
  i <- j[k]

  file_loc <- as.character(ImageTesting[i,1])
  imageName <- as.character(ImageTesting[i,2])

  imageRAW <- tiff_import(fileName = imageName, fileLoc = file_loc)
  image <- .horizontal_image_check(imageRAW)
  imagecut <- .trim_top_bottom(image, trimAmount = 20)
  imageSides <- .get_trace_start_ends(imagecut, returnMat = FALSE)
  imageProcessed <- .processImage(imagecut, cutoffQuantile = 0.95, cutoffProbability = 0.5)
  imageMatrix <- imageProcessed$gaussImageMatrix
  topcut <- .top_image_cut(imageMatrix = imageMatrix)
  imageMatrixTopCut <- imageMatrix[-c(0:topcut),]

  # Open a pdf file
  png(paste0("~/Magneto2020/plottingTesting/", imageName, ".png"))
  # 2. Create a plot
  suppressWarnings(plot(imageMatrix))
  abline(h = (nrow(imageMatrix) - topcut), col = "green")
  abline(v = c(imageSides$Start, imageSides$End), col = "green")
  text(imageSides$Start, x = 100, y = 1500, col = "green")
  text(imageSides$End, x = 5000, y = 1500, col = "green")
  # Close the pdf file
  dev.off()
}
