library("png")
library("knitr")
library("shiny")

pngs <- read.table("~/txtFiles/pngsFinal.txt")

for (i in pngs$V1) {
  userOpinion <- readline(prompt = "Is this image bright(b), good(g), dim(d)")

  dev.new()
  dev.set(2)
  OpenImageR::imageShow(i)
  dev.off(2)

  dev.set(1)
  scan <- readPNG(as.character(i))
  userOpinion <- readline(prompt = "Is this image bright(b), good(g), dim(d)")

  dev.off(1)y
}
