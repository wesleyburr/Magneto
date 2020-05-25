library("tiff")
library("pracma")
library("data.table")
library("functional")
library("OpenImageR")

#set working directory
setwd("~/Magneto2020/") #this is on the server
Image.Group <- paste0("~/magneto/Images/AGC-D-18980111-19010529")

# Main Scripts ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Main Scripts


source("~/Magneto2020/Scripts/MagDigitize.R")
#DWm6_final(file_location = file_location,Image_name = 0,withplots=TRUE,optimization=TRUE,saveresults=TRUE)
MagDigitize(file_location = Image.Group ,image_name = 0, withplots = FALSE ,optimization = TRUE ,saveresults = TRUE)


# Trace Identification through separation
#source("Scripts/MarkW/Hope_4Intersection1907.R")
#TIS(file_location = Image.Group, image_name = 0, withplots = TRUE, optimization = TRUE, saveresults = TRUE, bright = FALSE)

# Main Scripts ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Main Scripts




