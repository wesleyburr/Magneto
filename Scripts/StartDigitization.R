##DataFrom DigitizationTODO.py ---------------------------------------------------------
PWD <- setwd("~/Magneto2020/")

DigitizationTODO <- read.csv("~/Magneto2020/DataCSV/TestTable.csv", header = FALSE)
names(DigitizationTODO) <- c("ImagePath", "ImageName", "DigitizedYet", "DigitizationPath", "DigitizationName", "ErrorWhenDigitized")
Range = 19310908

##--------------------------------------------------------------------------------------

##MainScrips----------------------------------------------------------------------------
source("~/Magneto2020/Scripts/AutomationScript.R")
DigitizationUsingTIS(DigitizationTODO, PWD = PWD, keywordInName =  Range)

#Image.Group <- "~/SpecificDataAskedFor/ACG-D-193109/"
#source("~/Magneto2020/Scripts/TIS.R")
#TIS(file_location = Image.Group, image_name = "AGC-Z-19310908-19310910.tif", withplots = TRUE,
#    optimization = TRUE, saveresults = TRUE, bright = FALSE)
