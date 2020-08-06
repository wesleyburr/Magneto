library("magneto")
ImageTesting <- read.csv("~/Magneto2020/DataCSV/TODOBatch5.csv", header = TRUE, stringsAsFactors = FALSE)
k <- 1
j <- sample(1:26000, size = 100, replace = FALSE)


for (k in 1:100) {

  i <- j[k]
  file_loc <- as.character(ImageTesting[i,1])
  imageName <- as.character(ImageTesting[i,2])

  imageRAW <- tiff_import(fileName = imageName, fileLoc = file_loc)
  image <- magneto::.horizontal_image_check(imageRAW)
  imagecut <- magneto::.trim_top_bottom(image, trimAmountTop = 100, trimAmountBottom = 50)
  imageSides <- magneto::.get_trace_start_ends(imagecut, returnMat = FALSE, cutPercentage = 2)
  imageProcessed <- magneto::.process_image(imagecut, cutoffQuantile = 0.90, cutoffProbability = 0.4)
  imageMatrix <- imageProcessed$gaussImageMatrix
  imageWithoutSides <- imageMatrix[, -c(0:imageSides$Start, imageSides$End:ncol(imageMatrix))]
  topcut <- magneto::.top_image_cut(imageMatrix = imageWithoutSides, percentFromEdge = 2, percentEdgeForLeft = 25)
  bottomcut <- tryCatch(magneto::.bottom_image_cut(imageMatrix = imageWithoutSides, percentEdgeForLeft = 25,
                                                   percentFromEdge = 2, shortestAlowedSeqOfZeros = 15), warning = function(w) w)
  if (inherits(bottomcut, "warning")) {
    print(bottomcut)
    print(imageName)
    bottomcut <- ncol(imageMatrix) # this is the only way I know of doing this
  }
  imageMatrixTopCut <- imageMatrix[-c(0:topcut),]






  # Open a pdf file
  png(paste0("~/Magneto2020/plottingTesting/", imageName, ".png"))
  # 2. Create a plot
  suppressWarnings(plot(imageMatrix))
  abline(h = (nrow(imageMatrix) - topcut), col = "green")
  abline(h = (nrow(imageMatrix) - bottomcut), col = "green")
  abline(v = c(imageSides$Start, imageSides$End), col = "green")
  text(imageSides$Start, x = 800, y = 1500, col = "green")
  text(imageSides$End, x = 5000, y = 1500, col = "green")
  # Close the pdf file
  dev.off()
}
