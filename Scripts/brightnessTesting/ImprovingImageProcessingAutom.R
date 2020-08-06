
library("magneto")
library("OpenImageR")
library("rtiff")
library("zoo")

ImageTesting <- read.csv("~/Magneto2020/DataCSV/TODOBatch5.csv", header = TRUE, stringsAsFactors = FALSE)
k <- 1
j <- sample(1:26000, size = 100, replace = FALSE)

for (k in 1:50) {
  i <- j[k]
  #i <- 13506
  file_loc <- as.character(ImageTesting[i,1])
  imageName <- as.character(ImageTesting[i,2])

  imageRAW <- tiff_import(fileName = imageName, fileLoc = file_loc)
  image <- magneto::.horizontal_image_check(imageRAW)
  imagecut <- magneto::.trim_top_bottom(image, trimAmountTop = 100, trimAmountBottom = 50)
  imageSides <- magneto::.get_trace_start_ends(imagecut, returnMat = FALSE)
  imageMatrix = imagecut
  #ImageProcessing with new stuff adn old stuff
  beta0 = -2.774327
  beta1 = 51.91687
  cutoffProbability = 0.5
  NADefault = 0

  # Open a png file
  png(paste0("~/Magneto2020/plottingTesting/", imageName, "-origional",".png"))
  # 2. Create a plot
  suppressWarnings(plot(image))
  # Close the png file
  dev.off()

  bright <- bright(imageMatrix = imagecut, beta0 = beta0, beta1 = beta1, cutoffProbability = cutoffProbability, NADefault = NADefault)
  print(bright)
  if (bright == TRUE) {
    imageProcessed <- .for_bright_image(imageMatrix)$gaussImageMatrix
    #new
    Dialation <- delationErosion(imageMatrix, Filter = c(13,13), method = 'delation')
    imageProcessedNew <- image_thresholding(Dialation, thresh = 0.8)
  }
  if (bright == FALSE) {
    imageProcessed <- .not_bright_image(imageMatrix, cutoffQuantile = 0.95)$gaussImageMatrix
    #new
    Dialation <- delationErosion(imageMatrix, Filter = c(8,8), method = 'delation')
    imageProcessedNew <- image_thresholding(Dialation, thresh = 0.5)
  }

  imageMatrix <- imageProcessed
  imageMatrixNew <- imageProcessedNew
  #old

  imageWithoutSides <- imageMatrix[, -c(0:imageSides$Start, imageSides$End:ncol(imageMatrix))]
  topcut <- magneto::.top_image_cut(imageMatrix = imageWithoutSides, percentFromEdge = 2, percentEdgeForLeft = 25)
  bottomcut <- tryCatch(magneto::.bottom_image_cut(imageMatrix = imageWithoutSides, percentEdgeForLeft = 25,
                                                   percentFromEdge = 2, shortestAlowedSeqOfZeros = 15), warning = function(w) w)
  if (inherits(bottomcut, "warning")) {
    print(bottomcut)
    print(imageName)
    bottomcut <- ncol(imageMatrix) # this is the only way I know of doing this
  }
  #imageMatrixTopCut <- imageMatrix[-c(0:topcut),]

  # Open a png file
  png(paste0("~/Magneto2020/plottingTesting/", imageName, "-old", ".png"))
  # 2. Create a plot
  suppressWarnings(plot(imageMatrix))
  abline(h = (nrow(imageMatrix) - topcut), col = "green")
  abline(h = (nrow(imageMatrix) - bottomcut), col = "green")
  abline(v = c(imageSides$Start, imageSides$End), col = "green")
  text(imageSides$Start, x = 800, y = 1500, col = "green")
  text(imageSides$End, x = 5000, y = 1500, col = "green")
  # Close the pdf file
  dev.off()


  #new
  imageWithoutSidesNew <- imageMatrixNew[, -c(0:imageSides$Start, imageSides$End:ncol(imageMatrixNew))]
  topcutNew <- magneto::.top_image_cut(imageMatrix = imageWithoutSidesNew, percentFromEdge = 2, percentEdgeForLeft = 25)
  bottomcutNew <- tryCatch(magneto::.bottom_image_cut(imageMatrix = imageWithoutSidesNew, percentEdgeForLeft = 25,
                                                   percentFromEdge = 2, shortestAlowedSeqOfZeros = 15), warning = function(w) w)
  if (inherits(bottomcutNew, "warning")) {
    print(bottomcutNew)
    print(imageName)
    bottomcutNew <- ncol(imageMatrixNew) # this is the only way I know of doing this
  }
  #imageMatrixNewTopCut <- imageMatrixNew[-c(0:topcutNew),]



  # Open a pdf file
  png(paste0("~/Magneto2020/plottingTesting/", imageName,"-new", ".png"))
  # 2. Create a plot
  suppressWarnings(plot(imageMatrixNew))
  abline(h = (nrow(imageMatrix) - topcutNew), col = "green")
  abline(h = (nrow(imageMatrix) - bottomcutNew), col = "green")
  abline(v = c(imageSides$Start, imageSides$End), col = "green")
  text(imageSides$Start, x = 800, y = 1500, col = "green")
  text(imageSides$End, x = 5000, y = 1500, col = "green")
  # Close the png file
  dev.off()
}
