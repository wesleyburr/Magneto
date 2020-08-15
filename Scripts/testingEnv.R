library("magneto")
library("zoo")
library("rtiff")
ImageTesting <- read.csv("~/Magneto2020/DataCSV/TODOBatch5.csv", header = TRUE, stringsAsFactors = FALSE)
k <- 1
#set.seed(1)
j <- sample(1:26000, size = 100, replace = FALSE)

flag = FALSE

for (k in 1:40) {

  i <- j[k]
  print(i)
  a <- Sys.time()
  file_loc <- as.character(ImageTesting[i,1])
  imageName <- as.character(ImageTesting[i,2])
  print(imageName)
  imageMatrix <- import_process_image(imageName = imageName, file_loc = file_loc)
  imagecut <- .trim_top_bottom(imageMatrix, trimAmountTop = 100, trimAmountBottom = 50) #takes off the usual flair spots
  imageSides <- .get_trace_start_ends(imagecut, returnMat = FALSE, cutPercentage = 2) # two vertical lines
  tripleBool <- .triple_check(imageMatrix = imageMatrix) #checking for triple trace images
  if (isTRUE(tripleBool)) {
    print("possible triple found!")
    next
  }

  imageWithoutSides <- imageMatrix[, -c(0:imageSides$Start, imageSides$End:ncol(imageMatrix))] #takes away the sides found above
  # finds top horizontal line
  topcut <- .top_image_cut(imageMatrix = imageWithoutSides, percentFromEdge = 2, percentEdgeForLeft = 25)
  #finds bottom horizontal line
  bottomcut <- tryCatch(magneto::.bottom_image_cut(imageMatrix = imageWithoutSides, percentEdgeForLeft = 25,
                                                   percentFromEdge = 2, shortestAlowedSeqOfZeros = 25), warning = function(w) w)

  #could be that no cut was found, both of those functions throw an error when this happens
  if (inherits(topcut, "warning") || inherits(bottomcut, "warning")) {
    print(bottomcut)
    print(imageName)
    flag = TRUE
    bottomcut <- nrow(imageMatrix) # this is the only way I know of doing this
  }
  #No warnings, then we can roll mean the image and work on the bounds
  else {#if (bottomcut != nrow(imageMatrix) & !inherits(topcut, "warning")) {

    rolledImage <- mean_roll_image(imageMatrix, topcut, bottomcut)
    #the envelopes
    #bottomEnvelope <- .bottom_env(rolledImage, sepDist = 10, max_roc = 50, maxNoise = 100)
    #bottomUpperEnv <- .bottom_upper_env(rolledImage, sepDist = 10, max_roc = 50, maxNoise = 100)
    #topLowerEnv <- .top_lower_env(rolledImage, sepDist = 10, max_roc = 50, maxNoise = 100)

    #topEnvelopeScaled <- nrow(imageMatrix) - bottomcut + topEnvelope
    #bottomEnvelopeScaled <- nrow(imageMatrix) - bottomcut + bottomEnvelope
    #topLowerEnvScaled <- nrow(imageMatrix) - bottomcut + topLowerEnv
    #bottomUpperEnvScaled <- nrow(imageMatrix) - bottomcut + bottomUpperEnv

    MatrixEnvelopes <- find_envelopes(rolledImage = rolledImage, imageMatrix = imageMatrix,
                                    bottomcut = bottomcut, returnType = "MatrixScaled")
    topEnvelopeMatrixScaled <- MatrixEnvelopes$TopEnvelope
    topLowerEnvelopeMatrixScaled <- MatrixEnvelopes$TopLowerEnvelope
    bottomUpperEnvelopeMatrixScaled <- MatrixEnvelopes$BottomUpperEnvelope
    bottomEnvelopeMatrixScaled <- MatrixEnvelopes$BottomEnvelope

    # topEnvelopeMatrixScaled <- bottomcut - topEnvelope
    # topLowerEnvelopeMatrixScaled <- bottomcut - topLowerEnv
    # bottomUpperEnvelopeMatrixScaled <- bottomcut - bottomUpperEnv
    # bottomEnvelopeMatrixScaled <- bottomcut - bottomEnvelope


    topTraceMatrix <- .isolating_trace(imageMatrix, topEnvelopeMatrixScaled,
                                       topLowerEnvelopeMatrixScaled)
    bottomTraceMatrix <- .isolating_trace(imageMatrix, bottomUpperEnvelopeMatrixScaled,
                                          bottomEnvelopeMatrixScaled)

    .envStartEnds <- function(traceMatrix, thresh = 200){
      possibleStarts <- vector()
      for (i in 1:ncol(traceMatrix)) {

        if (sum(traceMatrix[,i]) == 0) {
          possibleStarts <- append(possibleStarts, i)
        }
      }
      possibleEnds <- sort(possibleStarts, decreasing = TRUE)
      diffFoward <- diff(possibleStarts)
      diffReverse <- diff(possibleEnds)
      # getting the left side start
      diffStarts <- which(diffFoward > thresh) # which gap could be the gap to the other side of the trace
      if (length(diffStarts) == 0 || length(diffStarts) == Inf ) {
        StartPoint <- 0
      }
      else {
        StartPoint <- min(possibleStarts[diffStarts])

      }
      #going the other way to get the right side
      diffEnds <- which(abs(diffReverse) > thresh) # which gap could be the gap to the other side of the trace
      if (length(diffEnds) == 0 || length(diffEnds) == Inf ) {
        EndPoint <- 0
      }
      else {
        EndPoint <- min(possibleEnds[diffEnds])

      }
      if (EndPoint < StartPoint) {
        StartPoint <- 0
        EndPoint <- length(traceMatrix)
      }
      return(list(StartPoint = StartPoint, EndPoint = EndPoint))
    }

    TopStartsEnds <- .envStartEnds(topTraceMatrix)
    BottomStartsEnds <- .envStartEnds(bottomTraceMatrix)

    plotEnvelopes <- find_envelopes(rolledImage = rolledImage, imageMatrix = imageMatrix,
                                    bottomcut = bottomcut, returnType = "PlottingScaled")

    # Open a png file
    png(paste0("~/Magneto2020/plottingTesting/", imageName, ".png"))
    # 2. Create a plot
    suppressWarnings(plot(imageMatrix))
    lines(plotEnvelopes$TopEnvelope, col = "green")
    lines(plotEnvelopes$TopLowerEnvelope, col = "yellow")
    lines(plotEnvelopes$BottomUpperEnvelope, col = "green")
    lines(plotEnvelopes$BottomEnvelope, col = "yellow")
    abline(h = (nrow(imageMatrix) - topcut), col = "red")
    abline(h = (nrow(imageMatrix) - bottomcut), col = "red")
    # Close the pdf file
    dev.off()


    png(paste0("~/Magneto2020/plottingTesting/", imageName, "-TopTrace" ,".png"))
    # 2. Create a plot
    suppressWarnings(plot(topTraceMatrix))
    abline(v = c(TopStartsEnds$StartPoint, TopStartsEnds$EndPoint), col = "red")
    lines(plotEnvelopes$TopEnvelope, col = "green")
    lines(plotEnvelopes$TopLowerEnvelope, col = "yellow")
    # Close the pdf file
    dev.off()

    png(paste0("~/Magneto2020/plottingTesting/", imageName, "-BottomTrace", ".png"))
    # 2. Create a plot
    suppressWarnings(plot(bottomTraceMatrix))
    abline(v = c(BottomStartsEnds$StartPoint, BottomStartsEnds$EndPoint), col = "red")
    lines(plotEnvelopes$BottomUpperEnvelope, col = "green")
    lines(plotEnvelopes$BottomEnvelope, col = "yellow")
    # Close the pdf file
    dev.off()




    # if (imageSides$End >=  length(topLowerEnv)) {
    #   end <- length(topLowerEnv - 200) # 200 to keep away from the sides
    # }
    # else {
    #   end <- imageSides$End
    # }

    intersection <- tryCatch(.intersection_check(MatrixEnvelopes$TopEnvelope,
                                                 MatrixEnvelopes$BottomUpperEnv, imageName), warning = function(w) w)
    if (inherits(intersection, "warning")) {
      print(intersection)
    }
  }
  if (isTRUE(flag)) { # plotting so I can see whats wrong
    flag = FALSE
    # Open a pdf file
    png(paste0("~/Magneto2020/plottingTesting/", imageName, "failProcess", ".png"))
    # 2. Create a plot
    suppressWarnings(plot(imageMatrix))
    abline(h = (nrow(imageMatrix) - topcut), col = "green")
    abline(h = (nrow(imageMatrix) - bottomcut), col = "green")
    abline(v = c(imageSides$Start, imageSides$End), col = "green")
    text(imageSides$Start, x = 800, y = 1500, col = "green")
    text(imageSides$End, x = 5000, y = 1500, col = "green")
    text("This Isn't Processed", x = 3000, y = 1500, col = "orange")
    # Close the pdf file
    dev.off()
  }



  b <- Sys.time()
  print( b - a)
  print(" ")
  # # Open a pdf file
  # png(paste0("~/Magneto2020/plottingTesting/", imageName, ".png"))
  # # 2. Create a plot
  # suppressWarnings(plot(imageMatrix))
  # abline(h = (nrow(imageMatrix) - topcut), col = "green")
  # abline(h = (nrow(imageMatrix) - bottomcut), col = "green")
  # abline(v = c(imageSides$Start, imageSides$End), col = "green")
  # text(imageSides$Start, x = 800, y = 1500, col = "green")
  # text(imageSides$End, x = 5000, y = 1500, col = "green")
  # # Close the pdf file
  # dev.off()
}

