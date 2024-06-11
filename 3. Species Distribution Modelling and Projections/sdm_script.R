library(sdm)
library(dismo)
library(sp)

sdm_for_each_spp <- function(df) {
  library(sdm)
  library(dismo)
  
  # Define a function to set predictors based on conditions
  set_predictors <- function(water_column_use, distribution) {
    if (distribution == "global") {
      if (water_colummn_use == "pelagic") {
        return(atlanticsurf)
      } else if (water_use == "benthic") {
        return(bentsurf)
      }
    } else if (distribution == "north atlantic") {
      if (water_use == "pelagic") {
        return(northatlanticsurf)
      } else if (water_use == "benthic") {
        return(northatlanticbent)
      }
    } else if (distribution == "european") {
      if (water_use == "pelagic") {
        return(eurosurf)
      } else if (water_use == "benthic") {
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
    
    # Remove the unwanted column
    species_df$...1 <- NULL
    
    # Set predictors based on the conditions
    water_use <- unique(species_df$`water use`)
    distribution <- unique(species_df$distribution)
    
    predictors <- set_predictors(water_use, distribution)
    
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
results <- sdm_for_each_spp(mothersppdata)





