library("magneto")
library("zoo")
library("rtiff")
ImageTesting <- read.csv("~/Magneto2020/DataCSV/TODOBatch5.csv", header = TRUE, stringsAsFactors = FALSE)
k <- 1
#set.seed(1)
j <- sample(1:26000, size = 100, replace = FALSE)

flag = FALSE

for (k in 1:50) {

  i <- j[k]
  print(i)
  a <- Sys.time()
  file_loc <- as.character(ImageTesting[i,1])
  imageName <- as.character(ImageTesting[i,2])
  print(imageName)
  imageMatrix <- import_process_image(imageName = imageName, file_loc = file_loc)
  imageMatrix <- imageMatrix[, -c(0:(0.02*ncol(imageMatrix)), (ncol(imageMatrix) - 0.02*ncol(imageMatrix)):ncol(imageMatrix))]
  imagecut <- .trim_top_bottom(imageMatrix, trimAmountTop = 100, trimAmountBottom = 50) #takes off the usual flair spots
  imageSides <- .get_trace_start_ends(imagecut, returnMat = FALSE, cutPercentage = 2) # two vertical lines


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

    tripleBool <- .triple_check(imageMatrix = imageMatrix, topCut = topcut, bottomCut = bottomcut) #checking for triple trace images
    if (isTRUE(tripleBool)) {
      print("possible triple found!")
      next
    }


    rolledImage <- mean_roll_image(imageMatrix, topcut, bottomcut)

    MatrixEnvelopes <- find_envelopes(rolledImage = rolledImage, imageMatrix = imageMatrix,
                                    bottomcut = bottomcut, returnType = "MatrixScaled", maxNoise = 250, max_roc = 50)
    topEnvelopeMatrixScaled <- MatrixEnvelopes$TopEnvelope
    topLowerEnvelopeMatrixScaled <- MatrixEnvelopes$TopLowerEnvelope
    bottomUpperEnvelopeMatrixScaled <- MatrixEnvelopes$BottomUpperEnvelope
    bottomEnvelopeMatrixScaled <- MatrixEnvelopes$BottomEnvelope



    topTraceMatrix <- .isolating_trace(imageMatrix, topEnvelopeMatrixScaled,
                                       topLowerEnvelopeMatrixScaled)
    bottomTraceMatrix <- .isolating_trace(imageMatrix, bottomUpperEnvelopeMatrixScaled,
                                          bottomEnvelopeMatrixScaled)


    TopStartsEnds <- .env_start_end(topTraceMatrix, returnMatrix = FALSE)
    BottomStartsEnds <- .env_start_end(bottomTraceMatrix, returnMatrix = FALSE)

    TopEnvCut <- .env_start_end(topTraceMatrix, returnMatrix = TRUE)
    BottomEnvCut <- .env_start_end(bottomTraceMatrix, returnMatrix = TRUE)

    intersection <- tryCatch(.intersection_check(topEnv = MatrixEnvelopes$TopEnvelope,
                                                 bottomEnv = MatrixEnvelopes$BottomUpperEnv,
                                                 imageName, rmAmount = 1000), warning = function(w) w)
    if (inherits(intersection, "warning")) {
      print(intersection)
    }
    # I think that this could work at some point, but it is to inconsistent right now
    #need ot look at the key identification features of these collapsses
    # linesDown <- which((diff(bottomUpperEnvelopeMatrixScaled)) >= 100)
    # linesUp <- which((diff(bottomUpperEnvelopeMatrixScaled)) <= -100)
    #
    # if (length(linesUp) > 0) {
    #   #browser()
    #   correctingTerm <- bottomUpperEnvelopeMatrixScaled[linesUp]
    #   if (topEnvelopeMatrixScaled[linesUp + 1] + 2 >= bottomUpperEnvelopeMatrixScaled[linesUp + 1] &
    #       topEnvelopeMatrixScaled[linesUp + 1] - 2 <= bottomUpperEnvelopeMatrixScaled[linesUp + 1]) {
    #     #this is now on the top tracing so we need to correct
    #
    #     greaterThenCurrent <- which(linesDown > linesUp)
    #     if (length(greaterThenCurrent) > 0) {
    #       ending <- min(linesDown[greaterThenCurrent])
    #
    #       if (bottomUpperEnvelopeMatrixScaled[ending + 1] + 50 >= bottomUpperEnvelopeMatrixScaled[ending + 1] &
    #         bottomUpperEnvelopeMatrixScaled[ending + 1] - 50 <= bottomUpperEnvelopeMatrixScaled[ending + 1]) {
    #         for (k in linesUp:ending) {
    #           bottomUpperEnvelopeMatrixScaled[k] <- correctingTerm
    #         }
    #       }
    #     }
    #   }
    # }





    # create_trace <- function(traceMatrix, start, end, topEnv, bottomEnv, thresh = 5, MARange = 6, region = 2){
    #   traceLine <- vector()
    #   len = 4
    #   for (i in start:end) {
    #     column <- traceMatrix[,i]
    #     trace <- which(column == 1)
    #     if (length(trace) > 0) {
    #       top <- trace[1]
    #       bottom <- trace[length(trace)]
    #       middleOfTrace <- round((top + bottom) / 2)
    #     }
    #     else {
    #       middleOfTrace <- round((topEnv[i] + bottomEnv[i]) / 2)
    #     }
    #     traceLine <- append(traceLine, middleOfTrace)
    #   }
    #   for (j in 1:len) {
    #     jumpsUp <- which(abs(diff(traceLine)) >= thresh) # to catch the spikes
    #     if (length(jumpsUp) > 0 ) {
    #       #browser()
    #       jumpsUp <- jumpsUp + 1 # correction so we land on the jumps not the one before the jump
    #       for (i in jumpsUp) {
    #         if (i < MARange) {
    #           traceLine[i] <- mean(traceLine[0:(i + MARange)]) #MA smoothing
    #         }
    #         else if ((i + MARange) > length(traceLine)) {
    #           traceLine[i] <- mean(traceLine[(i - MARange):length(traceLine)]) #MA smoothing
    #         }
    #         else{
    #           traceLine[(i - region):(i + region)] <- mean(traceLine[(i - region - MARange):(i + region + MARange)]) #MA smoothing on the region
    #         }
    #       }
    #     }
    #   }
    #   # jumpsDown <- which(diff(traceLine) <= -thresh)
    #   # if (length(jumpsDown) > 0 ) {
    #   #   browser()
    #   #   for (i in jumpsDown) {
    #   #     if (i < MARange) {
    #   #       traceLine[i] <- mean(traceLine[0:(i + MARange)]) #MA smoothing
    #   #     }
    #   #     else if ((i + MARange) > length(traceLine)) {
    #   #       traceLine[i] <- mean(traceLine[(i - MARange):length(traceLine)]) #MA smoothing
    #   #     }
    #   #     else{
    #   #       traceLine[(i - region):(i + region)] <- mean(traceLine[(i - region - MARange):(i + region + MARange)]) #MA smoothing on the region
    #   #     }
    #   #   }
    #   # }
    #   return(traceLine) # no jumps, just returning the line no corrections
    # }

    #there are still jumps in the line, see why this is? ( might be upside down aswell ..)
    topTrace <- create_trace(topTraceMatrix, TopStartsEnds$Start, TopStartsEnds$End,
                            topEnvelopeMatrixScaled, topLowerEnvelopeMatrixScaled)
    bottomTrace <- create_trace(bottomTraceMatrix, BottomStartsEnds$Start, BottomStartsEnds$End,
                               bottomUpperEnvelopeMatrixScaled, bottomEnvelopeMatrixScaled)

    plotEnvelopes <- find_envelopes(rolledImage = rolledImage, imageMatrix = imageMatrix,
                                    bottomcut = bottomcut, returnType = "PlottingScaled", maxNoise = 100, max_roc = 25)

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
    #abline(v = TopStartsEnds, col = "green")
    # Close the pdf file
    dev.off()

    png(paste0("~/Magneto2020/plottingTesting/", imageName, "-BottomTrace", ".png"))
    # 2. Create a plot
    suppressWarnings(plot(bottomTraceMatrix[,BottomStartsEnds$Start:BottomStartsEnds$End]))
    #abline(v = c(BottomStartsEnds$StartPoint, BottomStartsEnds$EndPoint), col = "red")
    lines(plotEnvelopes$BottomUpperEnvelope[BottomStartsEnds$Start:BottomStartsEnds$End], col = "green")
    lines(plotEnvelopes$BottomEnvelope[BottomStartsEnds$Start:BottomStartsEnds$End], col = "yellow")
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

