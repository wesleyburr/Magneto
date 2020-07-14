#'Tiff File Check
#'
#'checks to see if the last part of a file name is .tif or .tiff
#'
#'@param character in the form something.tif
#'@return bool of TRUE or FALSE
#'@examples
#'.is_tiff(testing.tif)
.is_tiff <- function(character){
  lastStringInSplit <- strsplit(character, "")
  lenStrSp = length(lastStringInSplit[[1]])
  fileType <- lastStringInSplit[[1]][as.integer(lenStrSp - 3):lenStrSp]
  fileTypeOneString = paste0(fileType[1], fileType[2], fileType[3], fileType[4])
  if (fileTypeOneString == ".tif" | fileTypeOneString == "tiff") {
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
