

MagDigitize <- function(file_location = file_location, image_name = FALSE, numTraces = 2, withplots=TRUE,
                        optimization = TRUE, saveresults = TRUE){

  if(optimization == TRUE){
      source("~/Magneto2020/Scripts/BreakoutOptimization.R")
      }
  else{
      source("~/Magneto2020/Scripts/BreakoutNoOptimization.R")
    }




  source("~/Magneto2020/Scripts/CustomFunctions.R")

  library("tiff") # for reading and writing tiff files
  library("multitaper") # used in Decon
  library("pracma")
  library("functional")
  library("plyr")
  library("dplyr")
  library("OpenImageR")
  #library("adimpro")

  # Don't forget to set working directory!! :)

  # Read in image data
  # These image sets have had the text files removed

  # file.names <- dir("Images/19010530-19040829/AGC-D-19010530-19040829", pattern =".tif")
  # image_import <- function(image){readTIFF(sprintf("Images/19010530-19040829/AGC-D-19010530-19040829/%s",image))}

  # file.names <- dir("Images/19040830-19071124/AGC-D-19040830-19071124", pattern =".tif")
  # image_import <- function(image){readTIFF(sprintf("Images/19040830-19071124/AGC-D-19040830-19071124/%s",image))}
  #
  # file.names <- dir("Images/19071125-19110201/AGC-D-19071125-19110201", pattern =".tif")
  # image_import <- function(image){readTIFF(sprintf("Images/19071125-19110201/AGC-D-19071125-19110201/%s",image))}
  #
  # file.names <- dir("Images/19071125-19110201/AGC-D-19071125-19110201", pattern =".tif")
  # image_import <- function(image){readTIFF(sprintf("Images/19071125-19110201/AGC-D-19071125-19110201/%s",image))}
  #
  # file.names <- dir("Images/19071125-19110201/AGC-D-19071125-19110201", pattern =".tif")
  # image_import <- function(image){readTIFF(sprintf("Images/19071125-19110201/AGC-D-19071125-19110201/%s",image))}
  #
  # file.names <- dir("Images/19071125-19110201/AGC-D-19071125-19110201", pattern =".tif")
  # image_import <- function(image){readTIFF(sprintf("Images/19071125-19110201/AGC-D-19071125-19110201/%s",image))}


  # Has problems with:
  # file.names <- dir("Images/18980111-19010529/AGC-D-18980111-19010529", pattern =".tif")
  # image_import <- function(image){readTIFF(sprintf("Images/18980111-19010529/AGC-D-18980111-19010529/%s",image))}

  #file_location <- "Images/19040830-19071124/AGC-D-19040830-19071124"
  #Image_name <- "AGC-D-19041228-19041230.tif"
  file.names <- dir(file_location, pattern =".tif")
  image_import <- function(image){readTIFF(paste0(file_location,"/",image))}

  part1_fails <- 0
  part2_fails <- 0
  part3_fails <- 0
  part4_fails <- 0
  part5_fails <- 0
  part6_fails <- 0

  total_fails <- vector(length = 6)
  total_fails[1] <- part1_fails
  total_fails[2] <- part2_fails
  total_fails[3] <- part3_fails
  total_fails[4] <- part4_fails
  total_fails[5] <- part5_fails
  total_fails[6] <- part6_fails


  # Used O because I know it has not been used ANYWHERE! No one would use it. Just to be cautious....

  if(image_name == FALSE){
    number_of_Images <- length(file.names)
  }else{
    number_of_Images <- 1
  }

  # Start!!!
  for ( O in 1:number_of_Images){
    if(number_of_Images == 1){
      mgdat <- image_import(image = image_name)
    }else{
      mgdat <- image_import(image = file.names[O])
    }
    print("===================================")
    # if(ncol(mgdat) < nrow(mgdat)){
    #   mag1 <- apply(t(mgdat),2,rev)
    # }
    # Some of the scanned images have areas that capture the background of the scanner. This often appears in a soild black box running along a side if the image.

    mgdat[mgdat<0] <- 0
    if(quantile(mgdat,0.5) < 0.11){
      mgdat[mgdat<quantile(mgdat,0.5)] <- mgdat[mgdat<quantile(mgdat,0.65)]+quantile(mgdat,0.7)
    }
    if(ncol(mgdat)>nrow(mgdat)){
      mgdat <- apply(t(mgdat),2,rev)
      mgdat <- apply(mgdat,1,rev)
      mgdat <- apply(mgdat,1,rev)
    }

    # Some of the scanned images have areas that capture the background of the scanner.
    # This often appears in a solid black box running along a side if the image.
    if(max(rowSums(mgdat)) == max(rowSums(mgdat)[(length(rowSums(mgdat))-100):length(rowSums(mgdat))])){
      check_scan <- rowSums(mgdat)
      aaa<-quantile(check_scan,0.95)
      #plot(check_scan,type = "l")
      #lines(c(rep(aaa,length(rowSums(mgdat)))))
      # Remove data from the intersection point
      over <- which(rowSums(mgdat)[(length(rowSums(mgdat))-100):length(rowSums(mgdat))]>aaa)
      mgdat <- mgdat[1:(length(rowSums(mgdat))-100+over[1]),]
    }

    # Compute the deconvolution, Part 1===========================================================================================================
    print("Starting Decon....")

    mgdat.dcG <-  t(apply( mgdat, MARGIN = 1, FUN = deconvGauss, sig = 5, kern.trunc = 0.05, nw = 3 ))  ### @@@ if sig is small, will create error (negative value in rep())
    print("Finished Decon")

    # Additonal preprocessing, the top writing is causing too many problems. Since the extremly high majority of images, even the ones with activity, almost never go very high.
    # Will set top to image mean AGC-D-19050609-19050611.tif
    #==============================
    #  testing11 <- readTIFF("Images/19040830-19071124/AGC-D-19040830-19071124/AGC-D-19041228-19041230.tif")
    #testing11 <- readTIFF("Images/18980111-19010529/AGC-D-18980111-19010529/AGC-D-18980117-18980119.tif")
    # #      testing11 <- readTIFF("Images/AGC-D-18980228-18980302.tif")
    #  testing11 <- readTIFF("Images/19010530-19040829/AGC-D-19010530-19040829/AGC-D-19010709-19010711.tif")
    #  mgdat <- testing11
    #  mgdat.dcG <-  t(apply( mgdat, MARGIN = 1, FUN = deconvGauss, sig = 5, kern.trunc = 0.05, nw = 3 ))
    #  numTraces <- 2
    #  withplots <- TRUE
    #=============================
    #mgdat.dcG[mgdat.dcG>=quantile(mgdat.dcG,0.95)] <- 0.5
    # mgdat.dcG[,(ncol(mgdat.dcG)-350):ncol(mgdat.dcG)] <- median(mgdat.dcG)
    # mgdat.dcG[,1:100] <- median(mgdat.dcG)
    #
    # Line bosting, does not work
    # hold <- mgdat.dcG[4000:5000,1000:1500]
    # hold[hold > quantile(mgdat.dcG,0.95)] <- 0.3#0.95*max(mgdat.dcG)
    # hold[hold <= quantile(mgdat.dcG,0.95)&hold >= quantile(mgdat.dcG,0.91)] <- 0.3#0.95*max(mgdat.dcG)
    # hold[hold <= quantile(mgdat.dcG,0.9)&hold >= quantile(mgdat.dcG,0.85)] <- 0.2#0.8*max(mgdat.dcG)
    # hold[hold <= quantile(mgdat.dcG,0.84)&hold >= quantile(mgdat.dcG,0.7)] <- 0.1#0.95*max(mgdat.dcG)
    # mgdat.dcG[4000:5000,1000:1500] <- hold
    # writeTIFF(mgdat,"testing.tif")

    # Get the starting points for tracing, Part 2==============================================================================================
    flag <- FALSE
    quant <- 0.95
    threshold <- quantile(mgdat.dcG, quant)

    # hh <- sum(colSums(mgdat.dcG[,500:1000]))
    # hh2 <- sum(colSums(mgdat.dcG[,1000:1500]))
    # rs <- rowSums(mgdat.dcG)
    # plot(rs,type = "l")
    # baselineStarts <- findpeaks(hh[1:500], npeaks = 2, minpeakheight = 0.6*max(hh[1:500]))
    # findpeaks(hh[500:1500],npeaks = 2,minpeakheight = 0.5*max(hh[500:1500]),minpeakdistance= 100)
    # loc <- round( seq( 1, ncol(mgdat.dcG), length.out = 10 + 2 )[-c(1,10+2)] )

    ## @@@
    #test.sts <- try(getStarts( t(mgdat.dcG), nTraces = 2, threshold = threshold ))
    #print(nrow(test.sts))
    #if(nrow(test.sts) == 0){
    #  fails <- fails + 1
    #  next
    #}

    ## @@@

    try( while(!flag) {
      sts <- try(getStarts( t(mgdat.dcG), nTraces = numTraces, threshold = threshold ))
      if("try-error" %in% class(sts)){
        quant <- quant - 0.05
        threshold <- quantile(mgdat.dcG, quant)
      }else{
        if(dim(sts)[1] > 0) {
          flag <- TRUE
        } else {
          quant <- quant - 0.05
          threshold <- quantile(mgdat.dcG, quant)
        }}
    })

    if("try-error" %in% class(sts)){
      # This is to deal with situations where the writing might be causing failure
      mgdat.dcG[,(ncol(mgdat.dcG)-350):ncol(mgdat.dcG)] <- median(mgdat.dcG)
      mgdat.dcG[,1:100] <- median(mgdat.dcG)
      flag <- FALSE
      quant <- 0.95
      threshold <- quantile(mgdat.dcG, quant)
      try( while(!flag) {
        sts <- try(getStarts( t(mgdat.dcG), nTraces = numTraces, threshold = threshold ))
        if(dim(sts)[1] > 0) {
          flag <- TRUE
        } else {
          quant <- quant - 0.05
          threshold <- quantile(mgdat.dcG, quant)
        }
      })
    }

    try(if(nrow(sts) == 0){
      # This is to deal with situations where the writing might be causing failure
      mgdat.dcG[,(ncol(mgdat.dcG)-350):ncol(mgdat.dcG)] <- median(mgdat.dcG)
      mgdat.dcG[,1:100] <- median(mgdat.dcG)
      flag <- FALSE
      quant <- 0.95
      threshold <- quantile(mgdat.dcG, quant)
      try( while(!flag) {
        sts <- try(getStarts( t(mgdat.dcG), nTraces = numTraces, threshold = threshold ))
        if(dim(sts)[1] > 0) {
          flag <- TRUE
        } else {
          quant <- quant - 0.05
          threshold <- quantile(mgdat.dcG, quant)
        }
      })
    })


    # else{
    #   part2_fails <- part2_fails + 1
    #   total_fails[2] <- part2_fails
    #   print(sprintf("Number of fails = %s",total_fails))
    #   print("===================================")
    #   next
    # }

    #Having the print out helps stalls for some reason
    try(print(sts))
    try(if(nrow(sts)<=8){
      flag <- FALSE
      quant <- quant-0.05
      threshold <- quantile(mgdat.dcG, quant)
      try( while(!flag) {
        sts <- try(getStarts( t(mgdat.dcG), nTraces = numTraces, threshold = threshold ))
        if("try-error" %in% class(sts)){
          quant <- quant - 0.05
          threshold <- quantile(mgdat.dcG, quant)
        }else{
          if(dim(sts)[1] > 0) {
            flag <- TRUE
          } else {
            quant <- quant - 0.05
            threshold <- quantile(mgdat.dcG, quant)
          }}
      })
    })
    try(if(nrow(sts)<=8){
      flag <- FALSE
      quant <- quant-0.1
      threshold <- quantile(mgdat.dcG, quant)
      try( while(!flag) {
        sts <- try(getStarts( t(mgdat.dcG), nTraces = numTraces, threshold = threshold ))
        if("try-error" %in% class(sts)){
          quant <- quant - 0.05
          threshold <- quantile(mgdat.dcG, quant)
        }else{
          if(dim(sts)[1] > 0) {
            flag <- TRUE
          } else {
            quant <- quant - 0.05
            threshold <- quantile(mgdat.dcG, quant)
          }}
      })
    })
    try(print(sts))

    # Part 3 =============================================================================================================
    print("Starting getTraces function")

    mgdat.tr <- try(getTraces( t(mgdat.dcG), sts ))
    if("try-error" %in% class(mgdat.tr)){
      next
    }

    print("Finished getTraces")

    loi <- nrow(mgdat.dcG) #length of Image

    if(withplots==TRUE){
      print(sprintf("getTraces File Number %s",O))
      # Check performance
      plot(mgdat.tr[[1]]$x[1:loi], mgdat.tr[[1]]$y[1:loi],
           ylim = c(1, 1879), type = "l",main = file.names[O])
      lines(mgdat.tr[[1]]$x[(loi+1):(2*loi)], mgdat.tr[[1]]$y[(loi+1):(2*loi)])
      lines(mgdat.tr[[1]]$x[(2*loi+1):(3*loi)],mgdat.tr[[1]]$y[(2*loi+1):(3*loi)],col="red")
      lines(mgdat.tr[[1]]$x[(3*loi+1):(4*loi)], mgdat.tr[[1]]$y[(3*loi+1):(4*loi)])


      # if (is.element('2', mgdat.tr) == TRUE){
      # lines(mgdat.tr[[2]]$x[1:loi], mgdat.tr[[2]]$y[1:loi], col = "red")
      # lines(mgdat.tr[[2]]$x[(loi+1):(2*loi)], mgdat.tr[[2]]$y[(loi+1):(2*loi)], col = "blue")
      # lines(mgdat.tr[[2]]$x[(2*loi+1):(3*loi)], mgdat.tr[[2]]$y[(2*loi+1):(3*loi)], col = "red")
      # lines(mgdat.tr[[2]]$x[(3*loi+1):(4*loi)], mgdat.tr[[2]]$y[(3*loi+1):(4*loi)], col = "blue")
      # }
      #
      # if (is.element('3', mgdat.tr) == TRUE){
      #   k <- 10
      #   lines(mgdat.tr[[k]]$x[1:loi], mgdat.tr[[k]]$y[1:loi], col = "red")
      #   lines(mgdat.tr[[k]]$x[(loi+1):(2*loi)], mgdat.tr[[k]]$y[(loi+1):(2*loi)], col = "blue")
      #   lines(mgdat.tr[[k]]$x[(2*loi+1):(3*loi)], mgdat.tr[[k]]$y[(2*loi+1):(3*loi)], col = "red")
      #   lines(mgdat.tr[[k]]$x[(3*loi+1):(4*loi)], mgdat.tr[[k]]$y[(3*loi+1):(4*loi)], col = "blue")
      # }
      #
      # if (is.element('8', mgdat.tr) == TRUE){
      # lines(mgdat.tr[[8]]$x[1:loi], mgdat.tr[[8]]$y[1:loi], col = "blue")
      # lines(mgdat.tr[[8]]$x[loi:(2*loi)], mgdat.tr[[8]]$y[(loi):(2*loi)], col = "blue")
      # lines(mgdat.tr[[8]]$x[(2*loi):(3*loi)], mgdat.tr[[8]]$y[(2*loi):(3*loi)], col = "blue")
      # lines(mgdat.tr[[8]]$x[(3*loi):(4*loi)], mgdat.tr[[8]]$y[(3*loi):(4*loi)], col = "blue")
      # }
    }

    #There is the possibility that traces can begin where others end. Looks like a donkey Kong level... It really does
    # All traces should begin at 1, and go to the end of the image, need to trim starts, and ends

    # Now we have to check to see if a given trace has "lost its way" and became one with the borg, err an another trace.
    # This is common with images that have a single spike that perfectly fits into the other trace. +++++++++++++++++
    antiLoop <- 0
    anti1 <- c(1,2,3,4,5,6,7,8,9,10)
    if(numTraces==2){
      flag1 <- FALSE
      while(!flag1){
        testing_for_overlaps <- c(mgdat.tr[[1]]$y[(2*loi+1):(3*loi)]==mgdat.tr[[1]]$y[(3*loi+1):(4*loi)])
        same_value <- sum(testing_for_overlaps) # This should not be a large number
        print(same_value)
        if(same_value < 690){
          flag1 <- TRUE
        }

        if(anti1[9]==anti1[10]){
          print("Broken While loop")
          break
        }


        #Need to set up flag for threshold, some images need to perform this step, many times
        if(same_value>=690){
          # 500 selected incase of extreme event, but they should not be more than this
          # Find longest chain
          antiLoop <- antiLoop +1
          anti1[antiLoop] <- same_value

          trueChain <- 0
          for(qq in 1:length(testing_for_overlaps)){
            if(testing_for_overlaps[qq]==TRUE){
              trueChain <- trueChain + 1
            }else{
              trueChain <- 0
            }
            if(trueChain==690){
              postion_of_turnover <- qq-690
              break
            }
          }
          if(postion_of_turnover == 0){
            #They both move to the left
            # Need to add a solution. Use AGC-D-19051114-19051116.tif as an example
            # For now....
            flag1 <- TRUE
            break
          }

          # Now we need to identify what trace has lost its way, the means should be able to help us
          trace_1_mean <- mean(mgdat.tr[[1]]$y[(2*loi+1):(3*loi)])
          trace_2_mean <- mean(mgdat.tr[[1]]$y[(3*loi+1):(4*loi)])
          same_region_mean <- mean(mgdat.tr[[1]]$y[(2*loi+1):(3*loi)][postion_of_turnover:length(mgdat.tr[[1]]$y[(2*loi+1):(3*loi)])])
          if(abs(trace_1_mean-same_region_mean)>abs(trace_2_mean-same_region_mean)){
            # Trace1 has the greater distance, therefore the lost trace
            # Now we have to look at the other traces, and see if the number of points in common goes down, but we also must still be cautious of grey-scale value
            trace_1_max <- vector(length= length(mgdat.tr))
            trace_1_sum <- vector(length = length(mgdat.tr))
            for(yy in 1:length(mgdat.tr)){
              trace_1_max[yy] <- abs(mean(mgdat.tr[[yy]]$y[(2*loi+1+postion_of_turnover):(3*loi)])-same_region_mean)
              trace_1_sum[yy] <- sum(mgdat.tr[[yy]]$y[(2*loi+1):(3*loi)])
            }
            mgdat.tr[[1]]$y[(2*loi+1+postion_of_turnover):(3*loi)] <- mgdat.tr[[which(trace_1_max %in% max(trace_1_max))[1]]]$y[(2*loi+1+postion_of_turnover):(3*loi)]
          }else{
            # Trace2 has the greater distance, therefore the lost trace
            # Now we have to look at the other traces, and see if the number of points in common goes down
            trace_2_max <- vector(length= length(mgdat.tr))
            trace_2_sum <- vector(length = length(mgdat.tr))
            for(yy in 1:length(mgdat.tr)){
              trace_2_max[yy] <- abs(mean(mgdat.tr[[yy]]$y[(3*loi+1+postion_of_turnover):(4*loi)])-same_region_mean)
              trace_2_sum[yy] <- sum(mgdat.tr[[yy]]$y[(3*loi+1):(4*loi)])
            }
            mgdat.tr[[1]]$y[(3*loi+1+postion_of_turnover):(4*loi)] <- mgdat.tr[[which(trace_2_max %in% max(trace_2_max))[1]]]$y[(3*loi+1+postion_of_turnover):(4*loi)]
          }
        }
      }
    }

    #++++++++
    #writeTIFF(mgdat.dcG,"testing")
    print("Check1")



    # Part 4 ========================================================================
    # The Cleaning function can create many problems...
    no_traces <- FALSE
    mgdat.trClean <- try(cleanTraces( t(mgdat.dcG), mgdat.tr ))
    print("cleaningFunction")
    class(mgdat.trClean)
    if(is.list(mgdat.trClean)){
      print(sprintf("cleanTraces %s",O))
      # Check performance
      if(withplots==TRUE){
        p2 <- plot(mgdat.trClean[[1]]$x[1:loi], mgdat.trClean[[1]]$y[1:loi],
                   ylim = c(1, 1879), type = "l",main = file.names[O])
        lines(mgdat.trClean[[1]]$x[(loi+1):(2*loi)], mgdat.trClean[[1]]$y[(loi+1):(2*loi)])
        lines(mgdat.trClean[[1]]$x[(2*loi+1):(3*loi)], mgdat.trClean[[1]]$y[(2*loi+1):(3*loi)])
        lines(mgdat.trClean[[1]]$x[(3*loi+1):(4*loi)], mgdat.trClean[[1]]$y[(3*loi+1):(4*loi)])

        lines(mgdat.trClean[[2]]$x[1:loi], mgdat.trClean[[2]]$y[1:loi], col = "blue")
        lines(mgdat.trClean[[2]]$x[(loi+1):(2*loi)], mgdat.trClean[[2]]$y[(loi+1):(2*loi)], col = "blue")
        lines(mgdat.trClean[[2]]$x[(2*loi+1):(3*loi)], mgdat.trClean[[2]]$y[(2*loi+1):(3*loi)], col = "blue")
        lines(mgdat.trClean[[2]]$x[(3*loi+1):(4*loi)], mgdat.trClean[[2]]$y[(3*loi+1):(4*loi)], col = "blue")

        lines(mgdat.trClean[[3]]$x[1:loi], mgdat.trClean[[3]]$y[1:loi], col = "red")
        lines(mgdat.trClean[[3]]$x[(loi+1):(2*loi)], mgdat.trClean[[3]]$y[(loi+1):(2*loi)], col = "red")
        lines(mgdat.trClean[[3]]$x[(2*loi+1):(3*loi)], mgdat.trClean[[3]]$y[(2*loi+1):(3*loi)], col = "red")
        lines(mgdat.trClean[[3]]$x[(3*loi+1):(4*loi)], mgdat.trClean[[3]]$y[(3*loi+1):(4*loi)], col = "red")

        lines(mgdat.trClean[[4]]$x[1:loi], mgdat.trClean[[4]]$y[1:loi], col = "blue")
        lines(mgdat.trClean[[4]]$x[(loi+1):(2*loi)], mgdat.trClean[[4]]$y[(loi+1):(2*loi)], col = "blue")
        lines(mgdat.trClean[[4]]$x[(2*loi+1):(3*loi)], mgdat.trClean[[4]]$y[(2*loi+1):(3*loi)], col = "blue")
        lines(mgdat.trClean[[4]]$x[(3*loi+1):(4*loi)], mgdat.trClean[[4]]$y[(3*loi+1):(4*loi)], col = "blue")
      }
      # The follwing is to identify the cases where a trace is almost/completly removed by the function
      tr_3 <- mgdat.trClean[[3]]$z
      tr_4 <- mgdat.trClean[[4]]$z

      for(iii in 1:length(tr_3)){
        if(is.na(tr_3[iii])){
          tr_3[iii] <- 0
        }
        if(is.na(tr_4[iii])){
          tr_4[iii] <- 0
        }
      }

      if(length(tr_3[tr_3 > 0]) < 200 | length(tr_4[tr_4 >0]) < 200 ){
        no_traces <- TRUE
      }
    }else{
      print("cleaning failed")
      next}

    # Now we have to check to see if a given trace has "lost its way" into another trace



    ### NEW PART 5 AND 6 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Part 5 ========================================================================

    # mgdat.trBlRM <- try(removeBaseline( t(mgdat.dcG), mgdat.trClean ))
    mgdat.trBlRM <- c(1,2,3)
    # If the above fails: Below
    # Below is for the "nTraces = 2" case
    if(is.list(mgdat.trBlRM) == FALSE){
      mgdat.trBlRM <- rbind(mgdat.trClean[[3]],mgdat.trClean[[4]])
      for(p in 1:length(mgdat.trBlRM[,"x"])){
        if(mgdat.trBlRM[p,"trace"] == 3){mgdat.trBlRM[p,"trace"] <- 1}
        if(mgdat.trBlRM[p,"trace"] == 4){mgdat.trBlRM[p,"trace"] <- 2}
      }
    }

    if(withplots==TRUE){
      p3 <- try(plot(mgdat.trBlRM$x[1:nrow(mgdat.trBlRM)], mgdat.trBlRM$y[1:nrow(mgdat.trBlRM)],ylim = c(1, 1879), type = "l",main = file.names[O]))
    }
    # Baselines
    baselines <- rbind(mgdat.trClean[[1]],mgdat.trClean[[2]])
    baseline1 <- try(baselines[baselines$trace == 1,])
    baseline2 <- try(baselines[baselines$trace ==2,])

    if(withplots==TRUE){
      try(plot(baseline1$x[1:12472], baseline1$y[1:12472], ylim = c(1, 1879), type = "l",main = file.names[O]))
      try(plot(baseline2$x[1:12472], baseline2$y[1:12472], ylim = c(1, 1879), type = "l",main = file.names[O]))
    }

    baseline1$y[baseline1$y > quantile(baseline1$y,0.95,na.rm = TRUE)] <- NA
    baseline1$y[baseline1$y < quantile(baseline1$y,0.05,na.rm = TRUE)] <- NA

    baseline2$y[baseline2$y > quantile(baseline2$y,0.95,na.rm = TRUE)] <- NA
    baseline2$y[baseline2$y < quantile(baseline2$y,0.05,na.rm = TRUE)] <- NA

    b1Before <- baseline1
    b2Before <- baseline2

    baseline1 <- try(Remove_ends(baseline1))
    baseline2 <- try(Remove_ends(baseline2))

    #test for baseline removal (not what we wanted here)
    if(sum(!is.na(baseline1)) < sum(!is.na(b1Before))-1000){
      baseline1 <- b1Before

    }
    if(sum(!is.na(baseline2$y))<sum(!is.na(b2Before$y))-1000){
      baseline2 <- b2Before
      baseline2$y[baseline2$y > quantile(baseline2$y,0.95,na.rm = TRUE)] <- NA
      baseline2$y[baseline2$y < quantile(baseline2$y,0.05,na.rm = TRUE)] <- NA
    }

    if(no_traces==TRUE){
      # Use baselines to clip data from the orginal traces
      if(length(mgdat.tr)<2){next}
      if(numTraces==2){
        mgdat.trBlRM$y[1:loi]<- mgdat.tr[[2]]$y[(2*loi+1):(3*loi)]
        mgdat.trBlRM$y[(loi+1):(2*loi)] <- mgdat.tr[[2]]$y[(3*loi+1):(4*loi)]

      }
    }

    # Futher Cleaning
    # Separation
    day1 <- try(mgdat.trBlRM[mgdat.trBlRM$trace == 1,])
    day2 <- try(mgdat.trBlRM[mgdat.trBlRM$trace == 2,])
    if(withplots==TRUE){
      try(plot(day1$x, day1$y, ylim = c(1, 1879), type = "l",main = file.names[O]))
      try(plot(day2$x, day2$y, ylim = c(1, 1879), type = "l",main = file.names[O]))
    }

    try(if(nrow(day2)>7000){
      day2 <- day2[1:loi,]
      day2$x <- c(1:loi)
    })


    # Fill in any gaps that might have been removed from the cleaning function
    try(if(numTraces>=1){
      baseline1_withoutNAs <- which(!is.na(baseline1$y))
      day1$y[1:(baseline1_withoutNAs[1]-1)] <- NA
      day1$y[(baseline1_withoutNAs[length(baseline1_withoutNAs)]+1):length(day1$y)] <- NA
    })
    # Clean things up
    day1$y <- linear_sub(trace = day1$y,nonNaStart = which(!is.na(baseline1$y))[1],nonNaEnd = which(!is.na(baseline1$y))[length(which(!is.na(baseline1$y)))])
    try(if(numTraces==2){
      baseline2_withoutNAs <- which(!is.na(baseline2$y))
      day2$y[1:(baseline2_withoutNAs[1]-1)] <- NA
      day2$y[(baseline2_withoutNAs[length(baseline2_withoutNAs)]+1):length(day2$y)] <- NA
      # Clean things up
      day2$y <- linear_sub(trace = day2$y,nonNaStart = which(!is.na(baseline2$y))[1],nonNaEnd = which(!is.na(baseline2$y))[length(which(!is.na(baseline2$y)))])
    })
    #day1 <- try(remove_ends(day1))
    #day2 <- try(remove_ends(day2))

    if(withplots==TRUE){
      plot(day2$x, day2$y, ylim=c(1,1879), type = "l",xlab = "",ylab="",main = "")
      lines(day1$y, type = "l", col="red")
      lines(baseline2$y, type = "l")
      lines(baseline1$y, type = "l", col = "red")
    }

    Data_Length <- day1$x
    Trace1 <- day1$y
    #Trace2 <- day2$y[1:max(day2$x)]
    Trace2 <- day2$y

    Baseline1 <- baseline1$y
    Baseline2 <- baseline2$y

    # Now we need to correctly identify the timing marks,
    # thingy <- which(!is.na(Trace1))
    #
    # gather_timing_marksTrace1 <- vector(length=length(thingy))
    # gather_timing_marksBaseline1 <- vector(length=length(thingy))
    # for(ii in thingy[1]:tail(thingy,n=1)){
    #   gather_timing_marksTrace1[ii-thingy[1]+1] <- testing11[ii,Trace1[thingy[ii-thingy[1]+1]]]
    #   gather_timing_marksBaseline1[ii-thingy[1]+1] <- testing11[ii,Baseline1[thingy[ii-thingy[1]+1]]]
    # }
    #
    # plot(gather_timing_marksTrace1,type = "l")
    # lines(gather_timing_marksBaseline1, col="red")
    #
    # # One idea is to start somewhere in the middle to identify an estimate of the proper gap length
    # new_thingy <- c(gather_timing_marksTrace1)# = gather_timing_marksBaseline1)
    # newnewnew <- which(new_thingy<quantile(new_thingy,0.05))
    # new_thingy[newnewnew] <- 2
    # find_the_gaps <- findpeaks(new_thingy)
    #
    # plot(new_thingy,type="l")
    # points(find_the_gaps,col="red")



    if(numTraces==2){
      Data_Out <- cbind(Data_Length,Trace1,Trace2,Baseline1,Baseline2)
    }
    if(numTraces==1){
      Data_Out <- cbind(Data_Length,Trace1,Baseline1)
    }
    #======================================================= CSV
    if(saveresults == TRUE){
      filename <- file.names[O]
      #
      # name_of_file <- print(paste0("Digitized_Data/DWMark/",filename,".csv"))
      name_of_file <- print(paste0("Normal_Days_Per_month/",filename,".csv"))
      write.csv(Data_Out,file = name_of_file, row.names = FALSE)
    }
    # ======================================================== CSV

    # The following is the stiching process if we assume no time loss


    find_last_point_day1 <- try(last_point(day1))
    find_first_point_day2 <- try(first_point(day2))
    day2[,"y"] <- try(day2[,"y"] - (find_first_point_day2[1] - find_last_point_day1[1]))
    day2[,"x"] <- try(day2[,"x"] + (find_last_point_day1[2] - find_first_point_day2[2]))
    if(withplots==TRUE){
      try(plot(day2$x[1:12472], day2$y[1:12472], type = "l",main = file.names[O]))


      # Stiching them together Part 6 (New)
      mgdat.trBlRM <- try(rbind(day1,day2))
      finish <- mgdat.trBlRM
      try(plot(finish$x[1:nrow(finish)], finish$y[1:nrow(finish)], ylim = c(1, 1879), type = "l",main = print(paste(file.names[O],"| File Number",O))))
    }


    #woot2 <- Cseries(hh,2,3)
    #plot(data = woot2, x = woot2[,2], y = woot2[,3], xlim = c(1,12472), ylim = c(1,1879),type = "l")



    # Part 6 (Orignal) ========================================================================
    # Stitch traces together
    #mgTR <- try(stitchTraces( mgdat.trBlRM ))

    #p4 <- try(plot(mgTR$x[1:12472], mgTR$y[1:12472],ylim = c(1, 1879), type = "l",main = sprintf("Completed Image#%s %f",o,file.names[O])))


    print(sprintf("Number of fails = %s",total_fails))
    print("===================================")
  }}









# # New functions
# # Used in part 5
# remove_ends <- function(x){
#   for(j in 1:2){
#     testing <- (2/4)*nrow(x)
#     numb_na <- 0
#     if(j == 2){
#       x[,"y"] <- rev(x[,"y"])
#     }
#     #End of Line
#     for(i in testing:nrow(x)){
#       if(is.na(x[i,"y"])){
#         numb_na <- numb_na +1
#       }else{
#         numb_na = 0
#         next
#       }
#       if(numb_na >= 30){
#         x[i:nrow(x),"y"] <- NA
#         break
#       }
#     }
#   }
#   x[,"y"] <- rev(x[,"y"])
#   x
# }
#
# # Used in part 6
# first_point <- function(x){
#   for(i in 1:nrow(x)){
#     test <- x[i,"y"]
#     if(is.na(test) == FALSE){
#       y <- x[i,"y"]
#       x <- x[i,"x"]
#       return(cbind(y,x))
#     }
#   }
# }
#
# last_point <- function(x){
#   for(i in round((1/2)*nrow(x)):nrow(x)){
#     test <- x[i,"y"]
#     if(is.na(test) == TRUE){
#       y <- x[i-1,"y"]
#       x <- x[i-1,"x"]
#       return(cbind(y,x))
#     }
#   }
# }
#
# Cseries <- function(z, x = 0, y = 0,w = 0.5, o = 0.5){
#   #Help
#   #if(z == "?"){
#   #  message("Help Page ------------------------------------------------------------")
#   #  print("First element will be for your data, which needs to be of the list class")
#   #  print("Second is the numeric value of the column you wish to be your x-axis")
#   #  print("Third is the numeric value, of the column you wish to be your y-axis")
#   #  print("Works well with datasets that has NA values at the beginning, and end of the data")
#   #  return(message("End Help -------------------------------------------------------------"))
#   #}
#   # Initial Class Check
#   if(class(z) == "list"){
#   }else{
#     stop("Data is not of the List class")
#   }
#   # x and y check
#   if(x == 0 | y == 0){
#     warning("No x-axis, and or y-axis columns defined")
#   }
#
#   # Required Functions
#   last_point <- function(h,x,y,w=0.5){
#     for(k in round((w)*nrow(h)):nrow(h)){
#       test <- h[k,y]
#       if(is.na(test) == TRUE){
#         y_value <- h[k-1,y]
#         x_value <- h[k-1,x]
#         return(cbind(x_value,y_value))
#       }
#       if(k == nrow(h)){
#         y_value <- h[k,y]
#         x_value <- h[k,x]
#         return(cbind(x_value,y_value))
#       }
#     }}
#
#   first_point <- function(h,x,y,w=0.5){
#     for(j in 1:nrow(h)){
#       test <- h[j,y]
#       if(is.na(test) == FALSE){
#         y_value <- h[j,y]
#         x_value <- h[j,x]
#         return(cbind(x_value,y_value))
#       }
#     }
#   }
#
#   # Finding Start and End Points
#   for(i in 1:length(z)){
#     if(i == 1){
#       moving <- z[[1]]
#       next
#     }
#     if(i > 1 ){
#       z[[i]][,y] <- z[[i]][,y] - (first_point(z[[i]],2,3)[1,2] - last_point(moving,2,3)[1,2])
#       z[[i]][,x] <- z[[i]][,x] + (last_point(moving,2,3)[1,1] - first_point(z[[i]],2,3)[1,1])
#       moving <- rbind(moving,z[[i]])
#     }
#   }
#
#   return(moving)
#
# }
#
#
#
#
#
#
#
#
#
#
#
#























#
# # Part 5 ========================================================================
# mgdat.trBlRM <- try(removeBaseline( t(mgdat.dcG), mgdat.trClean ))
# p3 <- try(plot(mgdat.trBlRM$x[1:12472], mgdat.trBlRM$y[1:12472],
# ylim = c(1, 1879), type = "l",main = file.names[O]))
#
# # Part 6 ========================================================================
# # Stitch traces together
# mgTR <- try(stitchTraces( mgdat.trBlRM ))
#
#     p4 <- try(plot(mgTR$x[1:12472], mgTR$y[1:12472],
#                  ylim = c(1, 1879), type = "l",main = sprintf("Completed Image#%s %f",o,file.names[O])))
#
# print(sprintf("Number of fails = %s",fails))
# print("===================================")
# }
#
# for(i in 1:12472){
#   if(mgTR$trace[i] = 2){
#     mgTR$y[i] <- (mgTR$y[i] - 500)
#   }
# }
#
# p4 <- try(plot(mgTR$x[1:12472], mgTR$y[1:12472],
#                ylim = c(1, 1879), type = "l",main = sprintf("Completed %s",file.names[O])))











