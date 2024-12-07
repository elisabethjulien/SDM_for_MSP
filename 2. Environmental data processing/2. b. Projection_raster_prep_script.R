#load depth data as raster
file_path <- "~//depthraster.tif"
depth <- raster(file_path)

#load environmental data as rasters
tif_folder <- "~//environmental_data"
tif_files <- list.files(tif_folder, pattern = "\\.tif$", full.names = TRUE)
file_names <- basename(tif_files)
print(file_names)
#they all are either "benthic" or "surface" environmental data and it is specified in the file name

# Load each TIF file as a raster
raster_list <- lapply(tif_files, raster)

# Split into two lists based on whether the file names include "benthic" or not
benthic_files <- tif_files[grepl("Benthic", file_names, ignore.case = TRUE)]
surface_files <- tif_files[!grepl("Benthic", file_names, ignore.case = TRUE)]

#Filter files based on selected variables and time slices
pattern <- "(DissolvedMolecularOxygen BenthicMean Mean|pH BenthicMean Ltmin|TotalPhytoplankton BenthicMean Mean|Salinity BenthicMean Mean|OceanTemperature BenthicMean Ltmax).*2030-2040\\.tif$"
#below, "bent58525"'s name can be broken down into three components: bent = benthic environmental data, 585= SSP585, 35=2030-2040
bent58535 <- benthic_files[grep(pattern, benthic_files)]
raster_list <- lapply(bent58535, raster)
# Stack raster layers
bent58535 <- stack(raster_list)

#crop the stack to fit the extent of the Portuguese extended continental platform (ECP)
crop_extent<- (-45, -6, 28, 51)
ECPbent_585_35<-crop(bent58535, crop_extent)

#add depth to the raster stack
depth<-crop(depth, crop_extent)
ECPbent_585_35<-stack(ECPbent_585_35, depth)

#save raster
writeRaster(ECPbent_585_35, filename = "~//ECPbent_585_35.tif", format = "GTiff")
