#' Gaussian Deconvolution
#'
#' does a 1D deconvolution on a selected image matrix
#'
#' @param imageMatrix An imported image, can be imported with tiff_import()
#' @param sig Significance parameter, used in dnorm as the final quantile
#' @param kern.trunc Truncated value for the kernel
#' @param nw A positive double-precision number, the time-bandwidth parameter
#'
#' @return A gaussian deconvoluted image matrix
#' @export
deconv_gauss <- function(imageMatrix , sig = 10, kern.trunc = 0.05, nw = 4 ){
  # pad imageMatrix
  y <- numeric(stats::nextn( length(imageMatrix), factors = 2 ) )
  y[1:length(imageMatrix)] <- imageMatrix
  Y <- stats::fft(y)

  # Gaussian kernel
  g <- stats::dnorm( 0:(3*sig), sd = sig )
  # Pad kernel
  k <- numeric( length(Y) )
  k[1:length(g)] <- g
  k[length(k):(length(k) - length(g) + 2)] <- g[-1]
  k <- k / sum(k)
  K <- stats::fft(k)
  i <- abs(K) > kern.trunc

  # Deconvolution
  Y.K <- Y/K

  # Divide by zero problem: truncate for small values of kernel
  Y.K[-which(i)] <- 0

  # Window with dpss before inverse transform to avoid ripples
  if ( sum(i) > 2*nw ) {
    win <- multitaper::dpss(sum(i), nw = nw, k = 1)$v
    spl.i <- floor(length(win)/2)
    win.split <- c(win[-(1:spl.i)], rep(0, (length(Y.K) - length(win) - 1)), win[length(win):(spl.i + 1 )] )
    Y.K.win <- Y.K*win.split
  }else{
    warning( "Kernel width in Fourier domain too narrow for dpss bandwidth specified -- windowing abandoned" )
    Y.K.win <- Y.K
  }
  imageMatrix.dec.win <- (Re(stats::fft( Y.K.win, inverse = TRUE ))/length(y))[1:length(imageMatrix)]
  return(imageMatrix.dec.win)
}
