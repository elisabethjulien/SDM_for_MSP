library(dplyr)
library(readr)
library(raster)
library(terra)

#import all dowloaded csv files from GBIF
file_paths <- list.files(path = "://gbifs", pattern = "*.csv", full.names = TRUE)
data_list <- lapply(file_paths, read_delim, delim = "/t", quote = "///", escape_double = FALSE, trim_ws = TRUE)

#keep only data columns of interest, merge all files into one data frame
selected_data_list <- lapply(data_list[1:75], function(df) df[c("species", "decimalLatitude", "decimalLongitude")])
gbifdf <- bind_rows(selected_data_list)

#rename columns
gbifdf <- gbifdf %>%
  rename(lat = decimalLatitude, lon = decimalLongitude, species= species)

#import OBIS files, merge them by columns of interest
file_paths <- list.files(path = "://obis", pattern = "*.csv", full.names = TRUE)
file_paths <- list.files(path = "C:/Users/elili/Documents/M2 R/SMALL PELAGIC/csv creation/obis/New Folder", pattern = "*.csv", full.names = TRUE)
data_list <- lapply(file_paths, read_csv)
obisdf <- merge(data_list,data_list, by = c("scientificname", "decimallatitude", "decimallongitude"), all = TRUE)
obisdf <- obisdf %>% select(scientificname, decimallatitude, decimallongitude)

#rename columns
obisdf <- obisdf %>%
  rename(lat = decimallatitude, lon = decimallongitude, species= scientificname)

#merge obis and gbif data
df <- merge(df, obisb, by = c("scientificname", "decimallatitude", "decimallongitude"), all = TRUE)

#make sure latitude and longitude values are numeric
df$lon <- as.double(df$lon)
df$lat <- as.double(df$lat)

#inspect data for discrepancies (in species name) and correct if necessary
unique_values <- unique(df$species)
print(unique_values)
df <- df %>%
  mutate(species = ifelse(species == "Tursiops truncatus truncatus", "Tursiops truncatus", species))

#delete incomplete entries
df <- df[complete.cases(df$lat, df$lon), ]
#delete duplicate entries
df <- unique(df)

#replace space in species names by underscore for practicality
df$species <- gsub(" ", "_", df$species)

#filter out for land occurrences, using a marine environment raster 
file_path <- "://OceanTemperature Surface Mean 2010-2020.tif"
oceanrast <- raster(file_path)
oceanrast <- terra::rast(oceanrast)
extracted_values <- extract(oceanrast, cbind(df$lon, df$lat))
df <- df[extracted_values != 0, ]

#add water column use for each species
df <- df %>%
  mutate(
    water_column_use = case_when(
      species  %in% c("Acanthocybium_solandri",
                      "Balaenoptera_acutorostrata",
                      "Balaenoptera_borealis", 
                      "Balaenoptera_musculus",
                      "Balaenoptera_physalus",
                      "Caretta_caretta", 
                      "Cetorhinus_maximus",
                      "Coryphaena_hippurus",
                      "Decapterus_punctatus",
                      "Delphinus_delphis" ,
                      "Dermochelys_coriacea",
                      "Engraulis_encrasicolus" ,
                      "Eubalaena_glacialis",
                      "Globicephala_macrorhynchus",
                      "Grampus_griseus",
                      "Katsuwonus_pelamis" ,
                      "Lamna_nasus",
                      "Makaira_nigricans",
                      "Megaptera_novaeangliae",
                      "Orcinus_orca",
                      "Physeter_macrocephalus" ,
                      "Pseudorca_crassidens",
                      "Sardina_pilchardus",
                      "Scomber_scombrus",
                      "Seriola_dumerili",
                      "Sphyraena_barracuda",
                      "Stenella_coeruleoalba" ,
                      "Stenella_frontalis",
                      "Thunnus_alalunga" ,
                      "Thunnus_albacares",
                      "Thunnus_obesus" ,
                      "Thunnus_thynnus",
                      "Trachurus_trachurus",
                      "Tursiops_truncatus",
                      "Xiphias_gladius",
                      "Ziphius_cavirostris" 
      ) ~ "pelagic",
      species %in% c("Centroscymnus_coelolepis",
                     "Cymodocea_nodosa" ,
                     "Centrophorus_squamosus",
                     "Desmophyllum_pertusum" ,
                     "Dicentrarchus_labrax",
                     "Dipturus_batis",
                     "Hippocampus_guttulatus",
                     "Hippocampus_hippocampus",
                     "Hoplostethus_atlanticus",
                     "Laminaria_hyperborea",
                     "Laminaria_ochroleuca",
                     "Lithothamnion_corallioides",
                     "Loligo_vulgaris",
                     "Merluccius_merluccius",
                     "Mytilus_galloprovincialis",
                     "Octopus_vulgaris",
                     "Ostrea_edulis",
                     "Pagellus_bogaraveo",
                     "Palaemon_serratus",
                     "Phyllariopsis_brevipes",
                     "Phymatolithon_calcareum",
                     "Pollicipes_pollicipes",
                     "Raja_clavata",
                     "Raja_montagui",
                     "Rostroraja_alba",
                     "Ruditapes_decussatus",
                     "Saccharina_latissima",
                     "Saccorhiza_polyschides",
                     "Scophthalmus_maximus",
                     "Solea_solea",
                     "Sparus_aurata",
                     "Squalus_acanthias",
                     "Squatina_squatina",
                     "Undaria_pinnatifida",
                     "Venerupis_corrugata",
                     "Zeus_faber",
                     "Zostera_marina",
                     "Lophius_piscatorius"
      )   ~ "benthic_demersal",
      TRUE ~ NA_character_  # Assigns NA to other species not specified
    ))

#add distribution scale for each species
...

#save df as csv
write.csv(df, "://df.csv", row.names = FALSE)
