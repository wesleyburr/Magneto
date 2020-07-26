# MarkBreakOut2_1.R

########## Part 1 ======================================================================================================
deconvGauss <- function(x, sig = 10, kern.trunc = 0.05, nw = 4 ){
  require(multitaper)
  # Try 1d deconvolution

  # pad x
  y <- numeric( nextn( length(x), factors = 2 ) )
  y[1:length(x)] <- x
  Y <- fft(y)

  # Gaussian kernel
  g <- dnorm( 0:(3*sig), sd = sig )
  # Pad kernel
  k <- numeric( length(y) )
  k[1:length(g)] <- g
  k[length(k):(length(k)-length(g)+2)] <- g[-1]
  k <- k / sum(k)
  K <- fft(k)
  i <- abs(K) > kern.trunc

  # Deconvolution
  Y.K <- Y/K

  # Divide by zero problem: truncate for small values of kernel
  Y.K[-which(i)] <- 0

  # Window with dpss before inverse transform to avoid ripples
  if( sum(i) > 2*nw ){
    win <- dpss(sum(i), nw=nw, k=1)$v
    spl.i <- floor(length(win)/2)
    win.split <- c( win[-(1:spl.i)], rep(0,(length(Y.K)-length(win)-1)), win[length(win):(spl.i+1)] )
    Y.K.win <- Y.K*win.split
  }else{
    warning( "Kernel width in Fourier domain too narrow for dpss bandwidth specified -- windowing abandoned" )
    Y.K.win <- Y.K
  }

  x.dec.win <- (Re(fft( Y.K.win, inverse = TRUE ))/length(y))[1:length(x)]
  # x.dec <- (Re(fft( Y.K, inverse = TRUE ))/length(y))[1:length(x)]
  x.dec.win                                                                          ###
}

###### Part 2 ################################################################################################################################


getStarts <- function( x, nTraces, bl = TRUE, nStarts = 10
                       , loc = round( seq( 1, ncol(x), length.out = nStarts + 2 )[-c(1,nStarts+2)] )
                       , nups = round( nrow(x)/ 100 ), threshold = quantile(x,0.9), ... ){
  require( pracma )
  # Use the findpeaks function to find peaks in the candidate starting points
  npks <- nTraces*(bl+1) # How many peaks we want
  pksAll <- matrix( NA_real_, ncol = 5, nrow = nStarts * npks )

  for( i in 1:nStarts ){

    pks <- findpeaks(x[,loc[i]], nups = nups, threshold = threshold, npeaks = npks , sortstr = TRUE)

    # first column is height, second is the index where max, third and fourth are where peak begins, ends
    #write.table(x,file="testingx.txt")

    if( is.null(pks) || nrow(pks) < npks) {                               ### @@@
      next
    } else {
      # Sort the peaks by position, add an index
      pksAll[ (1+(i-1)*npks):(i*npks),] <- cbind( rep(i, npks), pks[ order(pks[,2]), ] )

    }
  }




  # Calculate peak widths
  pksAll <- cbind( pksAll, pksAll[,5] - pksAll[,4] )
  pksAll <- pksAll[ complete.cases(pksAll), ]

  # There is a problem when nrow(pksAll) can be zero


  # Optimize using vectors++++
  #   bline_fun <- function(i,newmatrix,numpeaks){
  #     k <- i-1
  #     lk <- length(k)
  #     x1 <- newmatrix[ seq( from = 1+k[1], to = nrow(newmatrix), by = numpeaks), ]
  #     if(lk>=2)
  #     x2 <- newmatrix[ seq( from = 1+k[2], to = nrow(newmatrix), by = numpeaks), ]
  #     if(lk>=3){
  #     x3 <- newmatrix[ seq( from = 1+k[3], to = nrow(newmatrix), by = numpeaks), ]
  #     }
  #     if(lk>=4){
  #     x4 <- newmatrix[ seq( from = 1+k[4], to = nrow(newmatrix), by = numpeaks), ]
  #     }
  #     # Output
  #     if(lk==1){
  #       list(x1)
  #     }
  #     if(lk==2){
  #       list(x1,x2)
  #     }
  #     if(lk==3){
  #       list(x1,x2,x3)
  #     }
  #     if(lk==4){
  #       list(x1,x2,x3,x4)
  #     }
  #   }
  #   louMeans_fun <- function(i,newmatrix){
  #     x1 <- tapply( 1:nrow(newmatrix[[1]]), INDEX = 1:nrow(newmatrix[[1]]), FUN = function(y) mean(y[-1]), y = newmatrix[[1]][,3] )
  #     x2 <- tapply( 1:nrow(newmatrix[[2]]), INDEX = 1:nrow(newmatrix[[2]]), FUN = function(y) mean(y[-2]), y = newmatrix[[2]][,3] )
  #     x3 <- tapply( 1:nrow(newmatrix[[3]]), INDEX = 1:nrow(newmatrix[[3]]), FUN = function(y) mean(y[-3]), y = newmatrix[[3]][,3] )
  #     x4 <- tapply( 1:nrow(newmatrix[[4]]), INDEX = 1:nrow(newmatrix[[4]]), FUN = function(y) mean(y[-4]), y = newmatrix[[4]][,3] )
  #     list(x1,x2,x3,x4)
  #   }
  #   keepi_fun <- function(i, newmatrix1, newmatrix2, nuPs){
  #     x1 <- newmatrix1[ abs( newmatrix2[[1]] - newmatrix1[[1]][,3] ) < nuPs, 1 ]
  #     x2 <- newmatrix1[ abs( newmatrix2[[2]] - newmatrix1[[2]][,3] ) < nuPs, 1 ]
  #     x3 <- newmatrix1[ abs( newmatrix2[[3]] - newmatrix1[[3]][,3] ) < nuPs, 1 ]
  #     x4 <- newmatrix1[ abs( newmatrix2[[4]] - newmatrix1[[4]][,3] ) < nuPs, 1 ]
  #     list(x1,x2,x3,x4)
  #   }
  #   pksAll_fun <- function(PKSALL,newmatrix2){
  #     x1 <- PKSALL[ PKSALL[ ,1] %in% newmatrix2, ]
  #   }
  #  # Opt
  #
  # bline  <- do.call(bline_fun,list(c(1:nTraces),pksAll,npks))
  # louMeans <- do.call(louMeans_fun,c(1:nTraces))
  # keepi <- do.call(keepi_fun,c(1:nTraces))
  # pksAll <- do.call(pksAll_fun,)

  if( bl & length( unique( pksAll[,1] ) ) > 2 ){
    for( i in 1:nTraces ){
      # Further reduce the starting locations by identifying baselines
      # We assume that all of the baselines are below all of the traces!!!
      bline <- pksAll[ seq( from = 1+(i-1), to = nrow(pksAll), by = npks), ]
      # Calculate leave-one-out means
      louMeans <- tapply( 1:nrow(bline), INDEX = 1:nrow(bline), FUN = function(i,y) mean(y[-i]), y = bline[,3] )
      # Check if each baseline candidate is more than nups away from louMeans
      keepi <- bline[ abs( louMeans - bline[,3] ) < nups, 1 ]
      # Only keep those start locations with seemingly stable baselines
      pksAll <- pksAll[ pksAll[ ,1] %in% keepi, ]
    }
  }
  # Put locs into pksAll
  pksAll[,1] <- loc[ pksAll[,1] ]
  colnames( pksAll ) <- c( "x", "z", "y", "peak.min", "peak.max", "peak.width" )
  attr( pksAll, "baselines" ) <- bl
  attr( pksAll, "nTraces" ) <- nTraces
  pksAll
}

slide <- function( i, maxSlide, z ){
  # Determine direction of slide
  d <- sign(diff( z[i+(-1:1)] ))
  #if(is.null(d[1]) || is.na(d[1])){d[1] <- 0} #### @@@@@@@
  #if(is.null(d[2]) || is.na(d[2])){d[2] <- 0} ### @@@@@@@

  if( d[1] < 0){ # Increasing to the left
    if( d[2] > 0){ # Increasing to the right
      warning( "Function increasing both left and right! Returning NA." )
      return( NA )  # Something weird has happened: We are in a trough!
    }else{ # Decreasing to the right
      mvd <- -1
    }
  }else{ # Decreasing to the left
    if( d[2] > 0){ # Increasing to the right
      mvd <- 1
    }else{ # Decreasing to the right
      return(i) # We are already at the local max
    }
  }
  # Calculate diff vector either left or right up to maxSlide steps
  z. <- z[ i:max( min( (i + mvd*maxSlide), length(z) ), 1) ]
  d <- min( which( sign( diff(z.) ) < 0 ) ) - 1
  if( !is.finite(d) ){
    warning( "No local maximum found within 'maxSlide' of current location. Returning NA.")
    d <- NA
  }
  i + mvd*d
}

#### Part 3 ##################################################################
require(data.table)

getTraces <- function( x, starts, maxMove.bw = 1/2 ){
  bl <- attr( starts, "baseline" )
  nTraces <- attr( starts, "nTraces" )
  locStarts <- unique( starts[,"x"] )
  nStarts <- length( locStarts )
  minPkWd <- min( starts[,"peak.width"] )
  #require("dplyr")

  # Break the starts into separate matrices, arrange in list
  # tL will be the state object
  tL <- vector( "list", length = nStarts )
  for( i in 1:(nStarts) ) {
    tL[[i]] <- starts[ starts[,"x"] == locStarts[i], c("x","y","z") ]
  }

  # Extend out each trace in both directions
  tLAll <- lapply( tL, function(x, nTraces, bl) data.frame( trace = 1:(nTraces*(bl+1)), x ), nTraces = nTraces, bl = bl )

  for( i in 1:nStarts){
    # trace all the way to the right, then all the way to the left
    while( tL[[i]][1, "x"] < ncol(x) ){#|| is.null(ncol(x))){ # to the right  ### @@@ Problems with ncol(x) being null. Maybe this is the reason? added || for experimenting
      loc <- tL[[i]][1,"x"]
      new.y <- mapply( slide, i = tL[[i]][,"y"], maxSlide = floor( minPkWd*maxMove.bw ), MoreArgs = list( z = x[, loc+1] ) )
      new.y[ is.na(new.y) ] <- tL[[i]][is.na(new.y), "y"] # If something weird happened, carry on...
      tL[[i]] <- cbind( x = rep(loc+1, length(new.y)), y = new.y,
                        z = x[new.y, loc+1])
      g <- list(tLAll[[i]], data.frame( trace = 1:(nTraces*(bl+1)),
                                        tL[[i]] ) )
      tLAll[[i]] <- as.data.frame(rbindlist(g))
      #tLAll[[i]] <- bind_rows( tLAll[[i]], data.frame( trace = 1:(nTraces*(bl+1)), tL[[i]] ) )
    }


    tL[[i]] <- starts[ starts[,"x"] == locStarts[i], c("x","y","z") ]
    while( tL[[i]][1,"x"] > 1 || is.null(ncol(x)) ){ # to the left
      loc <- tL[[i]][1,"x"]
      new.y <- mapply( slide, i = tL[[i]][,"y"], maxSlide = floor( minPkWd*maxMove.bw ), MoreArgs = list( z = x[,loc-1] ) )
      new.y[ is.na(new.y) ] <- tL[[i]][is.na(new.y),"y"] # If something weird happened, carry on...
      tL[[i]] <- cbind( x = rep(loc-1,length(new.y)), y = new.y, z = x[new.y,loc-1])
      #Old in MarkBreakOut.R
      #tLAll[[i]] <- rbind( tLAll[[i]], data.frame( trace = 1:(nTraces*(bl+1)), tL[[i]] ) )

      g <- list(tLAll[[i]], data.frame( trace = 1:(nTraces*(bl+1)),
                                        tL[[i]] ) )
      tLAll[[i]] <- as.data.frame(rbindlist(g))

      #tLAll[[i]] <- bind_rows( tLAll[[i]], data.frame( trace = 1:(nTraces*(bl+1)), tL[[i]] ) )
    }
    # Sort by trace, then by x value
    tLAll[[i]] <- tLAll[[i]][ order(tLAll[[i]]$trace, tLAll[[i]]$x), ]
  }
  attr( tLAll, "baseline" ) <- bl
  attr( tLAll, "nTraces" ) <- nTraces
  attr( tLAll, "locStarts" ) <- locStarts
  attr( tLAll, "minPkWd" ) <- minPkWd
  attr( tLAll, "maxMove.bw" ) <- maxMove.bw
  tLAll
}



getwidth <- function( pos, im, zval ){
  # Gets peak width in column of 'im' using consecutive values of z > zval
  z <- im[ , pos[2] ]
  d1 <- sign( diff( z[pos[1]:nrow(im)] ) )
  end1 <- min( min( which( d1 > 0 ) ), min( which( z[(pos[1]+1):nrow(im)] < zval ) ) ) - 1
  if( !is.finite(end1) ) end1 <- 0
  d2 <- sign( diff( z[pos[1]:1] ) )
  end2 <- min( min( which( d2 > 0 ) ), min( which( z[(pos[1]-1):1] < zval ) ) ) - 1
  if( !is.finite(end2) ) end2 <- 0
  end1 + end2
}



############## Part 4
##### Part 4 ##############################################################################################

cleanTraces <- function( x, traceList, calcGoodness, peakWidth.bw = 0.9, peakZval = quantile(x,0.9)  ){
  require( "plyr" )
  require( "pracma" )

  bl <- attr( traceList, "baseline" )
  #print(bl)
  nTraces <- attr( traceList, "nTraces" )
  locStarts <- attr( traceList, "locStarts" )
  minPkWd <- attr( traceList, "minPkWd" )
  maxMove.bw <- attr( traceList, "maxMove.bw" )
  maxSlide <- floor( minPkWd*maxMove.bw )

  if( missing( calcGoodness ) ) calcGoodness <- function(x) sum(x$z)
  if( is.null(bl)) bl <- FALSE  ### @@@@ it seems bl is not working
  if(is.null(nTraces)) nTraces <- 2

  names( traceList ) <- 1:length( traceList )
  out <- vector( "list", nTraces*(bl+1) )
  # Clean up traces...
  # Begin with baselines if available
  if( bl == TRUE ){    #### @@@@@@@@ Added "== TRUE"
    for( i in 1:nTraces ){
      # There should only be two possible traces between each start point
      # Q: Which trace looks more like the magnetogram, rather than writing, etc?
      # High peak values, proper trace width?
      trDF <- ldply( traceList, function(x, i) x[ x$trace == i, ], i = i, .id = "start" )
      # Break into chunks based on starting positions
      xBreaks <- c( 1, locStarts, max(trDF$x)+1 )
      trList <- vector( "list", length(xBreaks) )

      for( st in 1:(length(xBreaks)-1) ){
        trDF.chunk <- trDF[ trDF$x %in% xBreaks[st]:(xBreaks[st+1]-1), ]
        goodness <- ddply( trDF.chunk, ~ start, calcGoodness )
        # Extract the best trace
        trList[[st]] <- trDF.chunk[ trDF.chunk$start == goodness$start[ which.max( goodness$V1 ) ], ]
      }
      trDF <- ldply( trList )

      # Trim ends by setting y to NA
      # Start from the leftmost and rightmost start points
      # Use the peak widths from the deconvolved image to assess when cut-offs should be made
      # minPkWd is the minimum peak width from the starts, use minPkWd*peakWidth.bw as cut-off

      # Get peak width all along path
      wd <- apply( as.matrix(trDF[,c("y","x")]), MARGIN = 1, FUN = getwidth, im = x, zval = peakZval )
      trDF$wd <- wd

      # Trim ends off using peak widths -- should be 0 or very small on ends
      trDF[ -(min( which( trDF$wd > 2 ) ):max( which( trDF$wd > 2 ) )), c("y","z") ] <- NA
      # Trim ends further
      trDF[ -(min(which( diff( trDF$z ) < 0 )):(max(which( diff( trDF$z ) > 0 ))+1 )), c("y","z") ] <- NA
      # Locate the timing marks
      gaps <- findintervals( 1, trDF$wd )
      gaps <- as.matrix(matrix( gaps[-c(1,length(gaps))], ncol = 2, byrow = TRUE ))
      gaps[, 1] <- gaps[, 1]+1

      # if findintervals actually finds 0 wd entries (which means a full vertical slice), then ...
      if(dim(gaps)[1] > 0) {
        # Not interested in gaps where y has been clipped
        keepgaps <- as.matrix(apply( gaps, 1, function(x, y) all( x %in% which( !is.na(y) ) ), y = trDF$y ))
        gaps <- gaps[ keepgaps, , drop = FALSE]
        timings <- rowMeans(gaps)

        # Trim gaps
        for( j in 1:nrow(gaps) ){
          # Trim left
          dl <- min( which( diff( trDF$wd[ gaps[j,1]:1 ] ) <= 0 ) ) - 1
          gaps[j, 1] <- gaps[j, 1] - dl
          # Trim right
          df <- min( which( diff( trDF$wd[ gaps[j,2]:nrow(trDF) ] ) <= 0 ) ) - 1
          gaps[j, 2] <- gaps[j, 2] + df
          if(gaps[j,2] - gaps[j,1] > 300 | gaps[j,1] > gaps[j,2]){
            next
          }
          trDF[ gaps[j, 1]:gaps[j, 2], c("y","z") ] <- NA
        }
        timings.2 <- rowMeans(gaps)
      } else { # otherwise, do this other method

        ################################################################################
        #
        #  New, Wes' code
        #
        wd <- trDF$wd
        wd[ -(min(which(wd > 0)):max(which(wd > 0))) ] <- NA
        wd2 <- -(wd - mean(wd, na.rm = TRUE))
        wd2 <- (c(0, wd2[-length(wd2)]) + wd2 + c(wd2[-1], 0)) / 3
        wd2[is.na(wd2)] <- 0.0
        pks <- findpeaks(wd2, nups = 2, ndowns = 2, npeaks = 10 , sortstr = TRUE)

        timings <- pks[, 2]
        timings.2 <- pks[, 2] + round(rnorm(dim(pks)[2], sd = 3))
      }
      list(timings = timings, timings.2 = timings.2)

      # TODO:
      # Baselines should be more-or-less straight
      # Baselines will have timing marks that should be close to regular
      attr( trDF, "timings" ) <- timings
      attr( trDF, "timings.2" ) <- timings.2
      attr( trDF, "is.baseline" ) <- TRUE

      out[[i]] <- trDF
    } # i in 1:nTraces
  } # If bl
  # Now for the magnetogram traces...
  for( i in ( 1:nTraces + (nTraces*bl) ) ){
    # There should only be two possible traces between each start point
    # Q: Which trace looks more like the magnetogram, rather than writing, etc?
    # High peak values, proper trace width?
    trDF <- ldply( traceList, function(x, i) x[ x$trace == i, ], i = i, .id = "start" )
    # Break into chunks based on starting positions
    xBreaks <- c( 1, locStarts, max(trDF$x)+1 )
    trList <- vector( "list", length(xBreaks) )

    #This loop below seems to cause problems in the second trace, added if statement, seems to work
    if (i < nTraces + nTraces*bl){
      for( st in 1:(length(xBreaks)-1) ){
        trDF.chunk <- trDF[ trDF$x %in% xBreaks[st]:(xBreaks[st+1]-1), ]
        goodness <- ddply( trDF.chunk, ~ start, calcGoodness )
        # Extract the best trace
        trList[[st]] <- trDF.chunk[ trDF.chunk$start == goodness$start[ which.max( goodness$V1 ) ], ]
      }
      trDF <- ldply( trList )
    }

    # Trim ends by setting y to NA
    # Start from the leftmost and rightmost start points
    # Use the peak widths from the deconvolved image to assess when cut-offs should be made
    # minPkWd is the minimum peak width from the starts, use minPkWd*peakWidth.bw as cut-off

    # Get peak width all along path
    wd <- apply( as.matrix(trDF[,c("y","x")]), MARGIN = 1, FUN = getwidth, im = x, zval = peakZval )
    trDF$wd <- wd

    # Trim ends off using peak widths -- should be 0 or very small on ends
    trDF[ -(min( which( trDF$wd > 2 ) ):max( which( trDF$wd > 2 ) )), c("y","z") ] <- NA
    # Trim ends further
    trDF[ -(min(which( diff( trDF$z ) < 0 )):(max(which( diff( trDF$z ) > 0 ))+1 )), c("y","z") ] <- NA

    # If available, trim ends further using baselines
    # We don't *know* which baseline goes with this trace, so be conservative
    if( bl ){
      # The range of x values in the baselines
      xran <- lapply( out, function(x) range( x$x[!is.na(x$y)] ) )
      xran <- ldply( xran )[1:nTraces,]
      xran <- min(xran[,1]):max(xran[,2])
      # Trim
      trDF[ -which( trDF$x %in% xran ), c("y","z") ] <- NA
    }

    # Locate the timing marks
    gaps <- findintervals( 1, trDF$wd )
    gaps <- matrix( gaps[-c(1,length(gaps))], ncol = 2, byrow = TRUE )
    gaps[,1] <- gaps[,1]+1
    # Not interested in gaps where y has been clipped
    keepgaps <- apply( gaps, 1, function(x, y) all( x %in% which( !is.na(y) ) ), y = trDF$y )
    gaps <- gaps[ keepgaps, ]
    timings <- rowMeans(gaps)
    # Trim gaps
    for( j in 1:nrow(gaps) ){
      # Trim left
      dl <- min( which( diff( trDF$wd[ gaps[j,1]:1 ] ) <= 0 ) ) - 1
      gaps[j,1] <- gaps[j,1] - dl
      # Trim right
      df <- min( which( diff( trDF$wd[ gaps[j,2]:nrow(trDF) ] ) <= 0 ) ) - 1
      gaps[j,2] <- gaps[j,2] + df
      trDF[ gaps[j,1]:gaps[j,2], c("y","z") ] <- NA
    }
    timings.2 <- rowMeans(gaps)

    attr( trDF, "timings" ) <- timings
    attr( trDF, "timings.2" ) <- timings.2
    attr( trDF, "is.baseline" ) <- FALSE

    out[[i]] <- trDF
  } # i in 1:nTraces

  attr( out, "baseline" ) <- bl
  attr( out, "nTraces" ) <- nTraces
  attr( out, "locStarts" ) <- locStarts
  attr( out, "minPkWd" ) <- minPkWd
  attr( out, "maxMove.bw" ) <- maxMove.bw


  out
}

## Part 5 ===================================================
# TODO: test with traces that cross each other

# Function to assign traces to baselines and output pixel values
removeBaseline <- function( x, trDFList, blAdjFunc ){
  require( "plyr" )

  bl <- attr( trDFList, "baseline" )
  if( !bl ) stop( "trDFList doesn't appear to have (a) baseline(s)!")
  nTraces <- attr( trDFList, "nTraces" )
  locStarts <- attr( trDFList, "locStarts" )
  minPkWd <- attr( trDFList, "minPkWd" )
  maxMove.bw <- attr( trDFList, "maxMove.bw" )

  if( missing( blAdjFunc ) ) blAdjFunc <- function(x) x

  # Step 1: Assign each trace interval to a baseline
  # Extract timings attributes
  timings <- lapply( trDFList, attr, which = "timings" )
  names( timings ) <- 1:length(timings)
  timings <- ldply( timings, .fun = function(x) data.frame( x = x ), .id = "trace" )
  timings.2 <- lapply( trDFList, attr, which = "timings.2" )
  names( timings.2 ) <- 1:length(timings.2)
  timings.2 <- ldply( timings.2, .fun = function(x) data.frame( x = x ), .id = "trace" )

  is.baseline <- unlist( lapply( trDFList, attr, which = "is.baseline") )
  timings$baseline <- is.baseline[timings$trace]
  timings.2$baseline <- is.baseline[timings.2$trace]

  # Looping over each trace timing, assign a baseline
  findBL <- function(x, x.m, dmax = 5){
    d <- abs( x.m$x - x["x"] )
    if( min(d) > dmax ) return( NA )
    else return( x.m$trace[ which.min( d ) ] )
  }
  timings$blmatch <- apply( subset( timings, select = x ), 1
                            , findBL, x.m = subset( timings, baseline ), dmax = 10 )
  timings.2$blmatch <- apply( subset( timings.2, select = x ), 1
                              , findBL, x.m = subset( timings.2, baseline ), dmax = 10 )

  timings <- timings[complete.cases( timings ),]
  timings.2 <- timings.2[complete.cases( timings.2 ),]

  # TODO: We should be able to untangle single trace crosses by looking at timing endpoints
  # Cut trace into sections
  # tmgs <- subset( timings, trace == tr & !is.na(blmatch), select = x )$x
  # trDFList[[tr]]$section <- cut( trDFList[[tr]]$x, c( tmgs, nrow(trDFList[[tr]]) ) )



  # For each trace, subtract appropriate baseline
  # ASSUME that we have untangled crossings already
  for( tr in which(!is.baseline) ){
    getBL <- unique( na.omit( timings$blmatch[ timings$trace == tr] ) )
    if( length(getBL) > 1 ) stop( "Unable to identify unique baseline for trace", tr)
    trDFList[[tr]]$y <- trDFList[[tr]]$y - trDFList[[getBL]]$y
    trDFList[[tr]]$timingMark <- trDFList[[tr]]$x %in% round( timings$x[ timings$blmatch == getBL & timings$trace == tr ] )
  }
  out <- ldply( trDFList )
  out <- subset( out, trace %in% which(!is.baseline), select = c( trace, x, y, timingMark ) )
  row.names(out) <- 1:nrow(out)
  out$trace <- out$trace - nTraces
  out
}

## Part 6 ================================================================================

stitchTraces <- function( x, method = "meanInterval", n = 3 ){
  # This function stiches multiple traces together
  # The default method is to compute the mean interval, for the neighbouring n intervals
  # and pad the stitch to that length
  # Assume for now that the traces are in order, so that the first trace comes first in time
  require( "plyr" )

  # Timing marks by trace
  tm <- subset( x, timingMark, select = c(trace, x) )
  tm <- tm[ order( tm$trace, tm$x ), ]

  # Get the target stitch lengths

  if( method == "meanInterval" ){
    mnI <- function(x, n){
      d1 <- diff( x$x[1:(n+1)] )
      d2 <- -diff(x$x[nrow(x):(nrow(x)+n+1)])
      # orignal# d2 <- -diff( x$x[nrow(x):(nrow(x)-n-1)] )
      data.frame( trace = x$trace[1:2], int = c(mean(d1), mean(d2)) )
    }
    stL <- ddply( tm, .variables = .(trace), .fun = mnI, n = n )
    stL <- rbind( stL[1,], stL, stL[nrow(stL),] )
    gr <- rep( 1:(nrow(stL)/2), each = 2 )
    stL <- aggregate( stL, list( gr = gr ), mean )[,-1]
    stL$int <- round( stL$int )
  }else{
    stop( "No such method implemented as yet!" )
  }
  # Now clip each time series so that there are the correct stitch lengths

  # Clip NA values from beginning and ends of each trace first
  x.L <- dlply( x, .variables = .(trace), .fun = function(x){x[ min(which(!is.na(x$y))):max(which(!is.na(x$y))), ] })
  # Now pad back to achieve target stitch lengths

  # Begin by padding the first trace from the left
  nToMark <- min( which( x.L[[1]]$timingMark ) )
  #@
  if(is.na(stL$int[1])){stL$int[1] <- nToMark + 1}
  #@
  padLength <- stL$int[1] - nToMark


  # Cobble together the padding

  padDF <- x.L[[1]][1:padLength,]
  padDF$y <- NA_integer_
  padDF$timingMark <- FALSE
  x.L[[1]] <- rbind( padDF , x.L[[1]] )

  # Now pad the last trace from the right
  # nToMark <- nrow( x.L[[length(x.L)]] ) - max( which( x.L[[length(x.L)]]$timingMark ) ), problem with returning -Inf. Added loop
  if(max(x.L[[length(x.L)]]$timingMark ) < 1){
    nToMark <- nrow( x.L[[length(x.L)]] )
  }else{nToMark <- nrow( x.L[[length(x.L)]] ) - max( which( x.L[[length(x.L)]]$timingMark ))}
  #@
  if(is.na(stL$int[nrow(stL)])){stL$int[nrow(stL)] <- nToMark + 1}
  #@
  padLength <- stL$int[nrow(stL)] - nToMark
  # Cobble together the padding
  padDF <- x.L[[length(x.L)]][1:padLength,]
  padDF$y <- NA_integer_
  padDF$timingMark <- FALSE
  x.L[[length(x.L)]] <- rbind( x.L[[length(x.L)]], padDF )

  # Now pad the in-between parts (stitching)
  for( i in 1:(length(x.L) - 1) ){
    j <- i + 0:1
    # Determine how far to each timing mark (from end of first trace and beginning of next trace)
    nToMark <- nrow( x.L[[j[1]]] ) - max( which( x.L[[j[1]]]$timingMark) ) # End of first trace
    nToMark.2 <- min( which( x.L[[j[2]]]$timingMark) ) # Beginning of next trace
    if(nToMark.2 == Inf){nToMark.2 <- 0}

    # Determine how much padding must be done
    if(is.na(stL$int[j[2]])){padLength <- 1
    }else{padLength <- stL$int[j[2]] - nToMark - nToMark.2}
    # Pad one of the traces by this amount (let's pad the first trace)
    # Cobble together the padding
    padDF <- x.L[[j[1]]][1:padLength,]
    padDF$y <- NA_integer_
    padDF$timingMark <- FALSE
    x.L[[j[1]]] <- bind_rows( x.L[[j[1]]], padDF )
  }
  # Glue all traces together
  out <- ldply( x.L )
  # Replace x column with continuous x
  out$x <- 1:nrow(out)
  out
}
