library(raster) #to process raster data

# Defining a rescale function for homogenising rasters with different scales to a scale of 0-1 to represent habitat suitability
rescale <- function(raster_layer) {
  # Get the min-max values
  minmax_r <- range(values(raster_layer), na.rm = TRUE)
  # Rescale
  rescaled_values <- (raster_layer - minmax_r[1]) / diff(minmax_r)
  return(rescaled_values)
}

# Create an empty list to store rescaled rasters
rescaled_raster_list <- list()

# List all the raster files in the directory of your raster outputs from SDM
raster_dir <- "C:/xxx/tutti"
raster_files <- list.files(raster_dir, pattern=".tif$", full.names=TRUE) 

#rescale them

for (file in raster_files) {
  # Import raster
  raster_data <- raster(file)
  # Rescale the raster
  rescaled_raster <- rescale(raster_data)
  # Store the rescaled raster in the list
  rescaled_raster_list[[basename(file)]] <- rescaled_raster
}

#chek it out
head(rescaled_raster_list)

#make stacks of the species' rasters by MSP use

#starting with "conservação", this was reproduced for projections of interest

conpres<-rescaled_raster_list[grep("^(balmus|censqu|dip|car|dercor|cetmax|hopatl|hip|lamnas|eub| squ|thuthy|raj|cencoe).*pres.tif$", names(rescaled_raster_list), value = TRUE)]
conpres<-stack(conpres)
conpres <- overlay(conpres, fun=sum)
conpres<-rescale(conpres)
plot(conpres, col=cols)
file_path <- "C:xxx/conpres.tif"
# Save the raster layer
writeRaster(conpres, file_path, format = "GTiff", overwrite = TRUE)

con24555<-rescaled_raster_list[grep("^(balmus|censqu|dip|car|dercor|cetmax|hopatl|hip|lamnas|eub| squ|thuthy|raj|cencoe).*24555.tif$", names(rescaled_raster_list), value = TRUE)]
con24555<-stack(con24555)
con24555 <- overlay(con24555, fun=sum)
con24555<-rescale(con24555)
plot(con24555, col=cols)
file_path <- "C:xxx/con24555.tif"
# Save the raster layer
writeRaster(con24555, file_path, format = "GTiff", overwrite = TRUE)

#44444444444444444# TURISMO #44444444444444444# 

turismopres<-rescaled_raster_list[grep("^(balacu|zip|pse|ste|balbor|balmus|balphy|phymac|del|tur|eub|gra|glomac|meg|orc).*pres.tif$", names(rescaled_raster_list), value = TRUE)]
turismopres<-stack(turismopres)
turismopres <- overlay(turismopres, fun=sum)
turismopres<-rescale(turismopres)
plot(turismopres, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/turismopres.tif"
# Save the raster layer
writeRaster(turismopres, file_path, format = "GTiff", overwrite = TRUE)

turismo24555<-rescaled_raster_list[grep("^(balacu|zip|pse|ste|balbor|balmus|balphy|phymac|del|tur|eub|gra|glomac|meg|orc).*24555.tif$", names(rescaled_raster_list), value = TRUE)]
turismo24555<-stack(turismo24555)
turismo24555 <- overlay(turismo24555, fun=sum)
turismo24555<-rescale(turismo24555)
plot(turismo24555, col=cols)
class(turismo24555)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/turismo24555.tif"
# Save the raster layer
writeRaster(turismo24555, file_path, format = "GTiff", overwrite = TRUE)

####AQUACULTURE####

aquapres<-rescaled_raster_list[grep("^(balmus|censqu|dip|car|dercor|cetmax|hopatl|hip|lamnas|eub| squ|thuthy|raj|cencoe).*pres.tif$", names(rescaled_raster_list), value = TRUE)]
aquapres<-stack(aquapres)
aquapres <- overlay(aquapres, fun=sum)
aquapres<-rescale(aquapres)
plot(aquapres, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/aquapres.tif"
# Save the raster layer
writeRaster(aquapres, file_path, format = "GTiff", overwrite = TRUE)

# etc.. further data explorations encouraged. 
#For more specifications on species selection per MSP use, consult Methods 



