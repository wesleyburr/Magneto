##DataFrom DigitizationTODO.py ---------------------------------------------------------
PWD <- setwd("~/Magneto2020/")

DigitizationTODO <- read.csv("~/Magneto2020/TestTable.csv", header = FALSE)
names(DigitizationTODO) <- c("ImagePath", "ImageName", "DigitizedYet", "DigitizationPath", "DigitizationName", "ErrorWhenDigitized")

##--------------------------------------------------------------------------------------

##MainScrips----------------------------------------------------------------------------
source("~/Magneto2020/Scripts/AutomationScript.R")
DigitizationUsingTIS(DigitizationTODO, PWD = PWD)

Image.Group <- "~/SpecificDataAskedFor/ACG-D-193109/"
source("~/Magneto2020/Scripts/TIS.R")
TIS(file_location = Image.Group, image_name = "AGC-Z-19310831-19310902.tif", withplots = TRUE,
    optimization = TRUE, saveresults = TRUE, bright = FALSE)
