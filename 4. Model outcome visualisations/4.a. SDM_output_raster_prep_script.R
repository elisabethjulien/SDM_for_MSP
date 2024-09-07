
# Load the raster package
library(raster)

# Define the rescale function
rescale <- function(raster_layer) {
  # Get the min-max values
  minmax_r <- range(values(raster_layer), na.rm = TRUE)
  
  # Rescale
  rescaled_values <- (raster_layer - minmax_r[1]) / diff(minmax_r)
  
  return(rescaled_values)
}

# Create an empty list to store rescaled rasters
rescaled_raster_list <- list()

raster_dir <- "C:/xxx/tutti"

# List all the raster files in the directory
raster_files <- list.files(raster_dir, pattern=".tif$", full.names=TRUE) 

for (file in raster_files) {
  # Import raster
  raster_data <- raster(file)
  
  # Rescale the raster
  rescaled_raster <- rescale(raster_data)
  
  # Store the rescaled raster in the list
  rescaled_raster_list[[basename(file)]] <- rescaled_raster
}

head(rescaled_raster_list)

#make stacks of the species' rasters by MSP use
#44444444444444444# conservação #44444444444444444# 

conpres<-rescaled_raster_list[grep("^(balmus|censqu|dip|car|dercor|cetmax|hopatl|hip|lamnas|eub| squ|thuthy|raj|cencoe).*pres.tif$", names(rescaled_raster_list), value = TRUE)]
conpres<-stack(conpres)
conpres <- overlay(conpres, fun=sum)
conpres<-rescale(conpres)
plot(conpres, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/conpres.tif"
# Save the raster layer
writeRaster(conpres, file_path, format = "GTiff", overwrite = TRUE)

con24555<-rescaled_raster_list[grep("^(balmus|censqu|dip|car|dercor|cetmax|hopatl|hip|lamnas|eub| squ|thuthy|raj|cencoe).*24555.tif$", names(rescaled_raster_list), value = TRUE)]
con24555<-stack(con24555)
con24555 <- overlay(con24555, fun=sum)
con24555<-rescale(con24555)
plot(con24555, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/con24555.tif"
# Save the raster layer
writeRaster(con24555, file_path, format = "GTiff", overwrite = TRUE)

con58555<-rescaled_raster_list[grep("^(balmus|censqu|dip|car|dercor|cetmax|hopatl|hip|lamnas|eub| squ|thuthy|raj|cencoe).*58555.tif$", names(rescaled_raster_list), value = TRUE)]
con58555<-stack(con58555)
con58555 <- overlay(con58555, fun=sum)
con58555<-rescale(con58555)
plot(con58555, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/con58555.tif"
# Save the raster layer
writeRaster(con58555, file_path, format = "GTiff", overwrite = TRUE)

con24595<-rescaled_raster_list[grep("^(balmus|censqu|dip|car|dercor|cetmax|hopatl|hip|lamnas|eub| squ|thuthy|raj|cencoe).*24595.tif$", names(rescaled_raster_list), value = TRUE)]
con24595<-stack(con24595)
con24595 <- overlay(con24595, fun=sum)
con24595<-rescale(con24595)
plot(con24595, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/con24595.tif"
# Save the raster layer
writeRaster(con24595, file_path, format = "GTiff", overwrite = TRUE)

con58595<-rescaled_raster_list[grep("^(balmus|censqu|dip|car|dercor|cetmax|hopatl|hip|lamnas|eub| squ|thuthy|raj|cencoe).*58595.tif$", names(rescaled_raster_list), value = TRUE)]
con58595<-stack(con58595)
con58595 <- overlay(con58595, fun=sum)
con58595<-rescale(con58595)
plot(con58595, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/con58595.tif"
# Save the raster layer
writeRaster(con58595, file_path, format = "GTiff", overwrite = TRUE)

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

turismo24595<-rescaled_raster_list[grep("^(balacu|zip|pse|ste|balbor|balmus|balphy|phymac|del|tur|eub|gra|glomac|meg|orc).*24595.tif$", names(rescaled_raster_list), value = TRUE)]
turismo24595<-stack(turismo24595)
turismo24595 <- overlay(turismo24595, fun=sum)
turismo24595<-rescale(turismo24595)
plot(turismo24595, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/turismo24595.tif"
# Save the raster layer
writeRaster(turismo24595, file_path, format = "GTiff", overwrite = TRUE)

turismo58555<-rescaled_raster_list[grep("^(balacu|zip|pse|ste|balbor|balmus|balphy|phymac|del|tur|eub|gra|glomac|meg|orc).*58555.tif$", names(rescaled_raster_list), value = TRUE)]
turismo58555<-stack(turismo58555)
turismo58555 <- overlay(turismo58555, fun=sum)
turismo58555<-rescale(turismo58555)
plot(turismo58555, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/turismo58555.tif"
# Save the raster layer
writeRaster(turismo58555, file_path, format = "GTiff", overwrite = TRUE)

turismo58595<-rescaled_raster_list[grep("^(balacu|zip|pse|ste|balbor|balmus|balphy|phymac|del|tur|eub|gra|glomac|meg|orc).*58595.tif$", names(rescaled_raster_list), value = TRUE)]
turismo58595<-stack(turismo58595)
turismo58595 <- overlay(turismo58595, fun=sum)
turismo58595<-rescale(turismo58595)
plot(turismo58595, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/turismo58595.tif"
# Save the raster layer
writeRaster(turismo58595, file_path, format = "GTiff", overwrite = TRUE)

####AQUACULTURE####

aquapres<-rescaled_raster_list[grep("^(balmus|censqu|dip|car|dercor|cetmax|hopatl|hip|lamnas|eub| squ|thuthy|raj|cencoe).*pres.tif$", names(rescaled_raster_list), value = TRUE)]
aquapres<-stack(aquapres)
aquapres <- overlay(aquapres, fun=sum)
aquapres<-rescale(aquapres)
plot(aquapres, col=cols)
file_path <- "C:/Users/elili/Desktop/1907BIGREZ/triés/aquapres.tif"
# Save the raster layer
writeRaster(aquapres, file_path, format = "GTiff", overwrite = TRUE)

# etc.. further data explorations encouraged



