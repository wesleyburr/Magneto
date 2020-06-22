

#' Finds all files that contain a specific string, in this case for finding files
#' that contain a specific year.
#' @param path Where you would like the function to look.
#' @param keyword The word that you are trying to find in file names.
#' @return  Vector with all paths for each item found with the keyword contained in it.
#' @export

findPathsForkeyword <- function(path = "~/", keyword = NULL){
  library("fs")
  if(is.null(keyword) || is.null(path)) {
    return(print("Must not have null in the specified argument"))
  }
  word = paste0("*",keyword,"*")
  spathWithKeyword = vector()

  symlinks <- fs::dir_ls(path = path , recurse = TRUE, type = "symlink")
  if (!identical(symlinks, character(0))) {
    symL <- length(symlinks)
    for (i in 1:symL) {
      spathWithKeyword[i] <- fs::dir_ls(path = symlinks[i], recurse = TRUE, type = "symlink")
    }

  }
  pathWithKeyword  <- fs::dir_ls(glob = word, path = path, recurse = TRUE, type = "any")

  allPathWithKeyword <- c(spathWithKeyword, pathWithKeyword)
  return(allPathWithKeyword)
}


test <- findPathsForkeyword(keyword = "1890012")
