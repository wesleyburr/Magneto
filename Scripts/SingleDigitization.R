library("tiff")
library("pracma")
library("data.table")
library("functional")
library("OpenImageR")

#set working directory
setwd("~/Magneto2020/") #this is on the server



# Image Group ======================================================================= Image Group

# When working from App
#Image.Group <- paste0("E:/ShinyApps/App_testing/Images")

# Image Group ======================================================================= Image Group
Image <- "AGC-D-19071225-19071227.tif"
# Main Scripts ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ Main Scripts                                       Main Scripts
#     DWm6.R (No dplyr)
#         DWm6.R With Plots:
source("Scripts/MarkW/DWm6_withPlots.R") # Set up to test a given image, do not fear the errors/warnings
DWm6_withPlots(file_location = Image.Group, Image_name = Image)

#         DWm6.R Without Plots:
source("Scripts/MarkW/DWm6_NoPlots.R") # Set up to run over the image group
DWm6_NoPlots(file_location = Image.Group)

#     Hope
#         Hope With Plots
#source()

#         Hope Without Plots
#source()
