library(raster)
# list files of present environmental data
lst <- list.files(path="~//environmental_variables", pattern='.tif$',full.names = T) 

#filter out environmental variables irrelevant to study eg:
lst <- lst[!grepl("SeaIceCover", lst)]

# Split into two lists based on whether it is benthic or surface data
file_names <- basename(lst)
benthic_files <- lst[grepl("Benthic", file_names, ignore.case = TRUE)]
surface_files <- lst[!grepl("Benthic", file_names, ignore.case = TRUE)]

#stack rasters in each list
benthic_present <- stack(benthic_files)
surface_present <- stack(surface_files)

#analyse for collinearity
...

#restack rasters without collinear variables
...

#crop the rasters of present environmental variables to different extents for model calibrations
atlantic_extent <- extent(c(-80, 40, -50, 75))  # xmin, xmax, ymin, ymax
atlanticbenth <- crop(benth,atlantic_extent)
atlanticbenth <- stack(atlanticbenth)
atlanticsurf <- crop(surf,atlantic_extent)
atlanticsurf <- stack(atlanticsurf)
northatlantic_extent <- extent(c(-80, 40, 20, 65))  # xmin, xmax, ymin, ymax
northatlanticbenth <- crop(benth,northatlantic_extent)
northatlanticbenth <- stack(northatlanticbenth)
northatlanticsurf <- crop(surf,northatlantic_extent)
northatlanticsurf <- stack(northatlanticsurf)
euro_extent <- extent(c(-15, 20, 25, 75))  # xmin, xmax, ymin, ymax
eurosurf <- crop(surf,euro_extent)
eurosurf <- stack(eurosurf)
eurobenth <- crop(benth,euro_extent)
eurobenth <- stack(eurobenth)
