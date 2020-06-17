#' Finds all files that contain a specific string, in this case for finding files
#' that contain a specific year
#' @param path Where you would like the function to look
#' @param keyword The word that you are trying to find in file names
#' @return



getImagesForDate <- function(path = "~/", keyword = NULL){

  library("fs")
  setwd("~/")
  word = paste0("*",keyword,"*")
  #temporary until I can access all of the things in the directory (access denied)
  #Images  <- fs::dir_ls(glob = word, path = "~/magneto/Images/", recurse = TRUE)
  Images  <- fs::dir_ls(glob = word, path = "~/", recurse = TRUE)
}


test <- getImagesForDate(keyword = "testSpace")
