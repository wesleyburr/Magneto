library("magneto")
library("zoo")
library("rtiff")
ImageTesting <- read.csv("~/Magneto2020/DataCSV/TODOBatch5.csv", header = TRUE, stringsAsFactors = FALSE)
k <- 1
j <- sample(1:26000, size = 100, replace = FALSE)
flag = FALSE

for (k in 1:2) {

  i <- j[k]
  print(i)
  a <- Sys.time()
  file_loc <- as.character(ImageTesting[i,1])
  imageName <- as.character(ImageTesting[i,2])
  print(imageName)
  imageMatrix <- import_process_image(imageName = imageName, file_loc = file_loc)
  imagecut <- .trim_top_bottom(image, trimAmountTop = 100, trimAmountBottom = 50) #takes off the usual flair spots
  imageSides <- .get_trace_start_ends(imagecut, returnMat = FALSE, cutPercentage = 2) # two vertical lines
  tripleBool <- .triple_check(imageMatrix = imageMatrix, threshold = 200)
  if (isTRUE(tripleBool)) {
    print("possible triple found!")
    next
  }

  #imageMatrix <- edge_detection(imagecut, method = "Sobel")
  imageWithoutSides <- imageMatrix[, -c(0:imageSides$Start, imageSides$End:ncol(imageMatrix))] #takes away the sides found above
  # finds top horizontal line
  topcut <- .top_image_cut(imageMatrix = imageWithoutSides, percentFromEdge = 2, percentEdgeForLeft = 25)
  #finds bottom horizontal line
  bottomcut <- tryCatch(magneto::.bottom_image_cut(imageMatrix = imageWithoutSides, percentEdgeForLeft = 25,
                                                   percentFromEdge = 2, shortestAlowedSeqOfZeros = 30), warning = function(w) w)

  #could be that no cut was found, both of those functions throw an error when this happens
  if (inherits(topcut, "warning") || inherits(bottomcut, "warning")) {
    print(bottomcut)
    print(imageName)
    flag = TRUE
    bottomcut <- nrow(imageMatrix) # this is the only way I know of doing this
  }
  #No warnings, then we can roll mean the image and work on the bounds
  else {#if (bottomcut != nrow(imageMatrix) & !inherits(topcut, "warning")) {
    imageWithoutTopBottom <- imageMatrix[-c(0:topcut, bottomcut:nrow(image)), ]
    vert <- t(imageWithoutTopBottom)
    rolledImage <- t(rollmean(vert, k = 40, fill = "extend"))
    rolledImage[which(rolledImage != 0)] <- 1

    #the envelopes
    topEnvelope <- .top_env(rolledImage, sepDist = 10, max_roc = 50, maxNoise = 100)
    topEnvelopeScaled <- nrow(imageMatrix) - bottomcut + topEnvelope
    bottomEnvelope <- .bottom_env(rolledImage, sepDist = 10, max_roc = 50, maxNoise = 100)
    bottomEnvelopeScaled <- nrow(imageMatrix) - bottomcut + bottomEnvelope
    topLowerEnv <- .top_lower_env(rolledImage, sepDist = 10, max_roc = 50, maxNoise = 100)
    topLowerEnvScaled <- nrow(imageMatrix) - bottomcut + topLowerEnv
    bottomUpperEnv <- .bottom_upper_env(rolledImage, sepDist = 10, max_roc = 50, maxNoise = 100)
    bottomUpperEnvScaled <- nrow(imageMatrix) - bottomcut + bottomUpperEnv


     # Open a png file
    png(paste0("~/Magneto2020/plottingTesting/", imageName, ".png"))
    # 2. Create a plot
    suppressWarnings(plot(imageMatrix))
    lines(topEnvelopeScaled, col = "green")
    lines(bottomEnvelopeScaled, col = "yellow")
    lines(topLowerEnvScaled, col = "green")
    lines(bottomUpperEnvScaled, col = "yellow")
    abline(h = (nrow(imageMatrix) - topcut), col = "red")
    abline(h = (nrow(imageMatrix) - bottomcut), col = "red")
    # Close the pdf file
    dev.off()

    # if (imageSides$End >=  length(topLowerEnv)) {
    #   end <- length(topLowerEnv - 200) # 200 to keep away from the sides
    # }
    # else {
    #   end <- imageSides$End
    # }
    for (m in 300:(min(c(length(bottomUpperEnv), length(topLowerEnv))) - 300)) { #Checking for intersection between the two lines
      if (topLowerEnv[m] <= bottomUpperEnv[m]) {
        print(warning(paste0("There is an intersection at (", topLowerEnv[m], ", ", m, ")")))
        print(imageName)
        break
      }
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

