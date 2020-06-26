#vector <- 1:3145

#selected <- sample(vector, size = 1000, replace = FALSE)

#images <- read.csv("~/Magneto2020/DataCSV/DigitizedB5Split.csv")

#for (i in selected) {
  #anImage <- read.csv(paste0(images[i,1], images[i,2]))
 # write.csv(x = anImage, file = paste0("~/Magneto2020/data/",as.character(images[i,2])))
#}

#' This function takes a .csv from digitizationTODO.py and randomly selects
#' .csv files from the list
#' @param TODOFileWithPath is a .csv from digitizationTODO.py
#' @param outSize Final size of files that are randomly selected=
#' @param outPath Where the files should be stored
randomSelection <- function(TODOFileWithPath, outSize, outPath){

  files <- read.csv(as.character(TODOFileWithPath))
  vector <- vector(length(files[,1]))
  selected <- sample(vector, size = outSize, replace = FALSE)

  for (i in selected) {
    oneFile <- read.csv(paste0(files[i,1], files[i,2]))
    write.csv(x = oneFile, file = paste0(outPath,as.character(files[i,2])))
  }
}
