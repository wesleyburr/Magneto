library("magneto")
library("zoo")
library("rtiff")
ImageTesting <- read.csv("~/Magneto2020/DataCSV/TODOBatch5.csv", header = TRUE, stringsAsFactors = FALSE)
k <- 1
#set.seed(1)
j <- sample(1:26000, size = 100, replace = FALSE)
browser()
flag = FALSE

for (k in 1:100) {

  i <- j[k]
  print(i)
  a <- Sys.time()
  file_loc <- as.character(ImageTesting[i,1])
  imageName <- as.character(ImageTesting[i,2])
  print(imageName)

  typeCheck <- tryCatch(.hdv_check(imageName), warning = function(w) w)
  if (inherits(typeCheck, "warning")) {
    print(typeCheck)
    print("Skipping to the next image")
    flag = TRUE
  }
  else {
    imageMatrix <- tryCatch(import_process_image(imageName = imageName, file_loc = file_loc), error = function(e) e)
    if (inherits(imageMatrix, "error")) {
      print(imageMatrix)
      next
    }
    imageSideCut <- trim_Sides(imageMatrix) # trims two perc from both sides
    imagecut <- trim_top_bottom(imageSideCut, trimAmountTop = 100, trimAmountBottom = 50) #takes off the usual flair spots

    imageSides <- .get_trace_start_ends(imagecut, returnMat = FALSE, cutPercentage = 2) # two vertical lines for top and bottom est
    imageWithoutSides <- imagecut[, -c(0:imageSides$Start, imageSides$End:ncol(imageSideCut))] #takes away the sides found above
    # finds top horizontal line
    topcut <- tryCatch(.top_image_cut(imageMatrix = imageWithoutSides, percentFromEdge = 2, percentEdgeForLeft = 25), warning = function(w) w)
    #finds bottom horizontal line
    bottomcut <- tryCatch(.bottom_image_cut(imageMatrix = imageWithoutSides, percentEdgeForLeft = 25,
                                                     percentFromEdge = 2, shortestAllowedSeqOfZeros = 25), warning = function(w) w)


    #could be that no cut was found, both of those functions throw an error when this happens
    if (inherits(bottomcut, "warning")) {
      print(bottomcut)
      print(imageName)
      flag = TRUE #causes a fail to process
      bottomcut <- nrow(imageMatrix) #reset to the bottom of the image
    }
    if (inherits(topcut, "warning")) {
      print(topcut)
      print(imageName)
      flag = TRUE #causes a fail to process
      topcut <- 0 #reset to the top of the image
    }
  }
  #No warnings, then we can roll mean the image and work on the bounds
  if (isFALSE(flag)) {#if (bottomcut != nrow(imageMatrix) & !inherits(topcut, "warning")) {

    tripleBool <- triple_check(imageMatrix = imageMatrix, topCut = topcut, bottomCut = bottomcut) #checking for triple trace images
    if (isTRUE(tripleBool)) {
      print("possible triple found!")
      next
    }


    rolledImage <- mean_roll_image(imagecut, topcut, bottomcut)

    MatrixEnvelopes <- find_envelopes( imageMatrix = imagecut, rolledImage = rolledImage,
                                    bottomCut = bottomcut, returnType = "MatrixScaled", maxNoise = 250, max_roc = 35)
    topEnvelopeMatrixScaled <- MatrixEnvelopes$TopEnvelope
    topLowerEnvelopeMatrixScaled <- MatrixEnvelopes$TopLowerEnvelope
    bottomUpperEnvelopeMatrixScaled <- MatrixEnvelopes$BottomUpperEnvelope
    bottomEnvelopeMatrixScaled <- MatrixEnvelopes$BottomEnvelope



    topTraceMatrix <- .isolating_trace(imagecut, topEnvelopeMatrixScaled,
                                       topLowerEnvelopeMatrixScaled)
    bottomTraceMatrix <- .isolating_trace(imagecut, bottomUpperEnvelopeMatrixScaled,
                                          bottomEnvelopeMatrixScaled)


    TopStartsEnds <- env_start_end(topTraceMatrix, returnMatrix = FALSE)
    BottomStartsEnds <- env_start_end(bottomTraceMatrix, returnMatrix = FALSE)

    #TopEnvCut <- .env_start_end(topTraceMatrix, returnMatrix = TRUE)
    #BottomEnvCut <- .env_start_end(bottomTraceMatrix, returnMatrix = TRUE)

    intersection <- tryCatch(.intersection_check(topEnv = MatrixEnvelopes$TopEnvelope,
                                                 bottomEnv = MatrixEnvelopes$BottomUpperEnv,
                                                 imageName, rmAmount = 1000), warning = function(w) w)
    if (inherits(intersection, "warning")) {
      print(intersection)
    }

    # ( might be upside down aswell ..)
    topTrace <- create_trace(traceMatrix = topTraceMatrix, start = TopStartsEnds$Start,
                            end = TopStartsEnds$End,
                            topEnv = topEnvelopeMatrixScaled,
                            bottomEnv =  topLowerEnvelopeMatrixScaled)
    bottomTrace <- create_trace(traceMatrix = bottomTraceMatrix,start =  BottomStartsEnds$Start,
                                end =  BottomStartsEnds$End,
                                topEnv = bottomUpperEnvelopeMatrixScaled,
                                bottomEnv =  bottomEnvelopeMatrixScaled)

    if (length(which(abs(diff(topTrace)) > 50)) > 0) {
      print("abnormal spike in the top trace")
    }
    if (length(which(abs(diff(bottomTrace)) > 50)) > 0) {
      print("abnormal spike in the bottom trace")
    }


    plotEnvelopes <- find_envelopes(rolledImage = rolledImage, imageMatrix = imagecut,
                                    bottomCut = bottomcut, returnType = "PlottingScaled", maxNoise = 250, max_roc = 35)

    # Open a png file
    png(paste0("~/Magneto2020/plottingTesting/", imageName, ".png"))
    # 2. Create a plot
    suppressWarnings(plot(imagecut))
    lines(plotEnvelopes$TopEnvelope, col = "green")
    lines(plotEnvelopes$TopLowerEnvelope, col = "yellow")
    lines(plotEnvelopes$BottomUpperEnvelope, col = "green")
    lines(plotEnvelopes$BottomEnvelope, col = "yellow")
    abline(h = (nrow(imagecut) - topcut), col = "red")
    abline(h = (nrow(imagecut) - bottomcut), col = "red")
    abline(v = TopStartsEnds, col = "green")
    abline(v = BottomStartsEnds, col = "orange")
    # Close the pdf file
    dev.off()


    png(paste0("~/Magneto2020/plottingTesting/", imageName, "-TopTrace" ,".png"))
    # 2. Create a plot
    suppressWarnings(plot(topTraceMatrix[,TopStartsEnds$Start:TopStartsEnds$End]))
    #abline(v = c(TopStartsEnds$StartPoint, TopStartsEnds$EndPoint), col = "red")
    lines(plotEnvelopes$TopEnvelope[TopStartsEnds$Start:TopStartsEnds$End], col = "green")
    lines(plotEnvelopes$TopLowerEnvelope[TopStartsEnds$Start:TopStartsEnds$End], col = "yellow")
    lines(topTrace, col = "red")
    #abline(v = TopStartsEnds, col = "green")
    # Close the pdf file
    dev.off()

    png(paste0("~/Magneto2020/plottingTesting/", imageName, "-BottomTrace", ".png"))
    # 2. Create a plot
    suppressWarnings(plot(bottomTraceMatrix[,BottomStartsEnds$Start:BottomStartsEnds$End]))
    #abline(v = c(BottomStartsEnds$StartPoint, BottomStartsEnds$EndPoint), col = "red")
    lines(plotEnvelopes$BottomUpperEnvelope[BottomStartsEnds$Start:BottomStartsEnds$End], col = "green")
    lines(plotEnvelopes$BottomEnvelope[BottomStartsEnds$Start:BottomStartsEnds$End], col = "yellow")
    lines(bottomTrace, col = "red")
    #abline(v = BottomStartsEnds, col = "orange")
    # Close the pdf file
    dev.off()


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

}
