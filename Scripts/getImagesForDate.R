

#' Finds all files that contain a specific string, in this case for finding files
#' that contain a specific year.
#' @param path Where you would like the function to look.
#' @param keyword The word that you are trying to find in file names.
#' @return  Vector with all paths for each item found with the keyword contained in it.
#' @export

findPathsForkeyword <- function(path = "~/", keyword = NULL){
  library("fs")
  if (is.null(keyword)) {
    return(stop("Must not have null in the specified argument"))
  }
  word = paste0("*",as.character(keyword),"*")

  spathWithKeyword = vector()
  spath = vector

  symlinks <- as.character(fs::dir_ls(path = path , recurse = TRUE, type = "symlink"))
  if (!identical(symlinks, character(0))) { #The character 0 is what is displayed if no symlinks show up
    symL <- length(symlinks)
    for (i in 1:symL) {
      spath <- c(spath, as.character(fs::dir_ls(path = symlinks[i], glob = word, recurse = TRUE, type = "file", fail = FALSE)))
    }

  }
  pathWithKeyword  <- fs::dir_ls(glob = word, path = path, recurse = TRUE, type = "any")

  allPathWithKeyword <- c(spath, pathWithKeyword)
  return(as.character(allPathWithKeyword[-1]))
}


test <- findPathsForkeyword(keyword = "AGC--H-19260115-19260117.tif")
print(test)
