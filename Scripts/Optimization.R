library(profvis)

# Optimization Scripts -------------------------------------------------------------- Optimization Scripts                               Optimization
# DWm6.R

#         DWm6.R without dplyr, single image
source("Scripts/MarkW/DWm6_NoPlots_single.R")
total_time_default <- system.time(DWm6_NoPlots_single(file_location = Image.Group, Image_name = Image))
profile_default <- profvis({DWm6_NoPlots_single(file_location = Image.Group, Image_name = Image)})

#         DWm6.R with dplyr, (changed: cbind->bind_cols,rbind->bind_rows)
source("Scripts/MarkW/DWm6_NoPlots_with_dplyr.R")
total_time_dplyr <- system.time(DWm6_NoPlots_with_dplyr(file_location = Image.Group, Image_name = Image))
dplyr_profile_dplyr <- profvis({DWm6_NoPlots_with_dplyr(file_location = Image.Group, Image_name = Image)})

# with plots
source("Scripts/MarkW/DWm6_dplyr_withPlots.R")
DWm6_dplyr_withPlots(file_location = Image.Group, Image_name = Image)

total_time_DW <- profvis({MagDigitize(file_location = Image.Group,image_name=Image,withplots=TRUE,optimization=TRUE,
                                      saveresults=FALSE)})
total_time_TIS <- profvis({TIS(file_location = Image.Group,image_name=Image,withplots=TRUE,
                               optimization=TRUE,saveresults=FALSE)})



# Optimization Scripts -------------------------------------------------------------- Optimization Scripts

