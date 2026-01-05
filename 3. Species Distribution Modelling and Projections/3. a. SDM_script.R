library(sdm)
installAll() #make sure you have all the dependencies of sdm package
library(dismo)
library(sp)

#building a function to SDM many species with a few variations, according to water column use, distribution range, and as many background points as there are occurrences of the species

sdm_for_each_spp <- function(df) {

  #set predictors based on conditions
  set_predictors <- function(water_column_use, distribution) {
    if (distribution == "global") {
      if (water_colummn_use == "pelagic") {
        return(atlanticsurf)
      } else if (water_colummn_use == "benthic") {
        return(bentsurf)
      }
    } else if (distribution == "north atlantic") {
      if (water_colummn_use == "pelagic") {
        return(northatlanticsurf)
      } else if (water_colummn_use == "benthic") {
        return(northatlanticbent)
      }
    } else if (distribution == "european") {
      if (water_colummn_use == "pelagic") {
        return(eurosurf)
      } else if (water_colummn_use == "benthic") {
        return(eurobenth)
      }
    }
    stop("Invalid combination of water use and distribution")
  }
  
  # Get unique species from the dataframe
  unique_species <- unique(df$species)
  
  results <- list()
  
  # Loop through each species
  for (species in unique_species) {
    species_df <- df[df$species == species, ]
    
    # Set predictors based on the conditions
    water_colummn_use <- unique(species_df$`water use`)
    distribution <- unique(species_df$distribution)
    
    predictors <- set_predictors(water_colummn_use, distribution)
    
    # Number of occurrences
    num_occurrences <- nrow(species_df)
    
    # Create sdmData
    d_species <- sdmData(species ~ ., train = species_df,
                         predictors = predictors,
                         parallelSetting = list(ncore = 7),
                         bg = list(method = 'eRandom', n = num_occurrences))
    
    # Fit the sdm model
    m_species <- sdm(formula = as.formula(paste(species, "~ .")),
                     data = d_species,
                     methods = c("bioclim.dismo", "brt", "gam", "glm", "mahal.dismo", "maxent",
                                 "maxlike", "mlp", "rf", "rpart", "svm"),
                     replication = 'sub',
                     test.p = 30,
                     n = 50,
                     parallelSetting = list(ncore = 7, method = 'parallel', fork = FALSE))
    
    # Store the result
    results[[species]] <- m_species
    
    # Set coordinates
    coordinates(species_df) <- c("lon", "lat")
  }
  
  return(results)
}

# Example usage
results <- sdm_for_each_spp(df)

#Projections egs
m_lamnas <- results[["Lamna nasus"]]
lamnas_pres<-ensemble(m_lamnas,surfeez, filename= "/path/to/project/lamnaspres.tif", setting=list(method='weighted',stat="tss",op=2), parallelSetting = list(ncore=7, method='parallel',fork=F))
lamnas_24525<-ensemble(m_lamnas,EEZsurf_245_55, filename= "/path/to/project/lamnas24525.tif", setting=list(method='weighted',stat="tss",op=2), parallelSetting = list(ncore=7, method='parallel',fork=F))
lamnas_24535<-ensemble(m_lamnas,EEZsurf_585_55, filename= "/path/to/project/lamnas24535.tif", setting=list(method='weighted',stat="tss",op=2), parallelSetting = list(ncore=7, method='parallel',fork=F))

#next step wil be organising and visulising the results (see section 4)


