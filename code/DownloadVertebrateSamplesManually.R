# This script is for retrieving and arranging small mammal data from Neotoma, for the paleoSDMs project

# 0. Setup ----
# install.packages('neotoma2')
# install.packages('tidyverse')
# install.packages('DT')

library(neotoma2)
library(tidyverse)
library(DT)

# 1. find the small mammal datasets relevant to the project ----

# find all vertebrate datasets
vertDatasets <- get_datasets(datasettype = "vertebrate fauna", limit = 10000)
# saveRDS(vertDatasets, file= "output/vertDatasets.RDS")
# vertDatasets <- readRDS(file= "output/vertDatasets.RDS")

vertDownloads <- get_downloads(vertDatasets)
# saveRDS(vertDownloads, file= "output/vertDownloads.RDS")
# vertDownloads <- readRDS(file= "output/vertDownloads.RDS")

vertDownloads_orig <- vertDownloads # save the in case you need to recover it later
# vertDownloads <- vertDownloads_orig

#### NOTE: problematic sites
# There are three problematic sites, so the script breaks on these sites
# siteid                     sitename      lat      long altitude
# 4951 Chief Joseph Dam Site 45OK18 48.00000 -119.3667      691
# 5700          Calf Island [19SU8] 42.36667  -71.0000        0
# 21496           Lost Chicken Creek 64.05333 -141.8767      549
# For the saved objects above, this filter works,but check to see if order changed with a new download
vertDownloads <- vertDownloads[-c(1418, 2160, 3812)] # these are deleted because they have issues

vertDownloads_coords <- coordinates(vertDownloads)

# download data sample by sample
# find all datasets that have a new Syverson Blois bounds chronology

datasetSamples <- data.frame() # create blank samples list for the dataset

for (i in 2161:length(vertDownloads)){
  
  # pull out relevant site metadata
  sitename <- vertDownloads[[i]]@sitename
  site_coords <- NULL
  site_coords$long <- vertDownloads_coords[i,'long']
  site_coords$lat <- vertDownloads_coords[i,'lat']
  ids <- getids(vertDownloads[i])
  elev <- vertDownloads[[i]]@altitude
  
  ncollunits <- length(unique(ids$collunitid)) # this stores the number of collunits within the site
  
  for (j in 1:ncollunits){
    
    depositionalenvironment <- vertDownloads[[i]]$collunits[[j]]@depositionalenvironment # pull out more metadata, stored at collunit level
    sample_metadata <- cbind(sitename, ids[j, c('siteid', 'collunitid')], site_coords, elev, depositionalenvironment) # create metadata object for later appending
    
    chrons <- chronologies(vertDownloads[[i]]$collunits[j])
    chronDat <- do.call(rbind, lapply(chrons@chronologies, as.data.frame))
    
      ndatasets <- length(vertDownloads[[i]]@collunits[[j]]@datasets) # how many datasets are within the collunit?
      
      for (k in 1:ndatasets){
        nsamples <- length(vertDownloads[[i]]@collunits[[j]]@datasets[[k]]@samples) # how many samples are within the dataset?
        
        for (m in 1:nsamples){
          
          # find the vertebrate fauna data
          sampObject <- vertDownloads[[i]]@collunits[[j]]@datasets[[k]]@samples[[m]] # isolate a single sample
          
          # only do the following if there is a new Syverson-Blois chronology. 
          if (length(which(sampObject@ages$chronologyname == "Syverson-Blois: bounds")) > 0){
            
            sampDF <- do.call(cbind, lapply(sampObject@datum, as.data.frame)) # convert the NISP/MNI/PA data to a table
            colnames(sampDF) <- names(sampObject@datum) # rename the dataframe
          
            agesTemp <- sampObject@ages[which(sampObject@ages$chronologyname == "Syverson-Blois: bounds"),] # pull out only those samples with a Syverson-Blois chron
            
            sampAges <- as.data.frame(matrix(nrow=nrow(sampDF), ncol=ncol(agesTemp)))
            colnames(sampAges) <- colnames(agesTemp)
            sampAges[,1:ncol(sampAges)] <- agesTemp
            
            # add on higher level metadata
            sample_metadata_repeated <- as.data.frame(matrix(nrow=nrow(sampDF), ncol=ncol(sample_metadata)))
            colnames(sample_metadata_repeated) <- colnames(sample_metadata)
            sample_metadata_repeated[,1:ncol(sample_metadata)] <- sample_metadata
            datasetid <- rep(vertDownloads[[i]]@collunits[[j]]@datasets[[k]]@datasetid, nrow(sampAges))
            sample_metadata_repeated$datasetid <- datasetid
            
            # merge the three dataframes together
            sampleDF_full <- bind_cols(sample_metadata_repeated, sampDF, sampAges)
            
            # bind this specific dataset to the full dataset list
            datasetSamples <- bind_rows(datasetSamples, sampleDF_full)
            cat("samples = ", nrow(sampleDF_full), "; cumul. samples = ", nrow(datasetSamples), fill=T) # track progress, mostly useful for testing with smaller number of sites
            
          } # close if statement for Syverson chrons 
        } # close m (samples)
      } # close k (datasets)
  } # close j (collunits)
} # close i (sites)






