# Tracking down sites with errors

library(neotoma2)
library(tidyverse)
library(DT)

# 1. Load saved objects ----

# find all vertebrate datasets
# vertDatasets <- get_datasets(datasettype = "vertebrate fauna", limit = 10000)
# saveRDS(vertDatasets, file= "output/vertDatasets.RDS")
vertDatasets <- readRDS(file= "output/vertDatasets.RDS")

# vertDownloads <- get_downloads(vertDatasets)
# saveRDS(vertDownloads, file= "output/vertDownloads.RDS")
vertDownloads <- readRDS(file= "output/vertDownloads.RDS")

vertDownloads_orig <- vertDownloads # save the in case you need to recover it later
# vertDownloads <- vertDownloads_orig

#### NOTE: problematic sites
# There are three problematic sites, so the script breaks on these sites
# siteid                     sitename      lat      long altitude
# 4951 Chief Joseph Dam Site 45OK18 48.00000 -119.3667      691
# 5700          Calf Island [19SU8] 42.36667  -71.0000        0
# 21496           Lost Chicken Creek 64.05333 -141.8767      549
# For the saved objects above, this filter works,but check to see if order changed with a new download
focalSites <- vertDownloads[c(1418, 2160, 3812)] # these are deleted because they have issues

focalSites_coords <- coordinates(focalSites)

# figure out where the error is
# find all datasets that have a new Syverson Blois bounds chronology

datasetSamples <- data.frame() # create blank samples list for the dataset

# site notes  ----
i <- 1 # Chief Joseph Dam Site 45OK18
# one site, one collunit, one dataset
# three chrons: FAUNMAP1.1, two Syverson-Blois.
# 4 samples total. First three samples work fine. 4th sample, get an error:
# Error in `[<-.data.frame`(`*tmp*`, , 1:ncol(sampAges), value = list(age = c(NA_integer_,  : 
#                                                                               replacement element 1 has 2 rows, need 13
# This line is what has the error: sampAges[,1:ncol(sampAges)] <- agesTemp
# Issue is with sample ages. There appear to be two separate sets of sample ages for the two Syverson chronologies:
# > sampObject
# An object of class "sample"
# Slot "ages":
#   age                         agetype ageolder ageyounger chronologyid         chronologyname
# 1   NA Calibrated radiocarbon years BP     5624       1194        48157 Syverson-Blois: bounds
# 2   NA Calibrated radiocarbon years BP     6000       3967        48157 Syverson-Blois: bounds # incorrect ages?
# 3   NA Calibrated radiocarbon years BP     6000       3967        48158  Syverson-Blois: event # incorrect ages?
# 4   NA            Radiocarbon years BP     3512       2000         3625            FAUNMAP 1.1
# 5 3374 Calibrated radiocarbon years BP     4144       2674        48158  Syverson-Blois: event


i <- 2 # "Calf Island [19SU8]"
# one site, one collunit, one dataset
# three chrons: FAUNMAP1.1, two Syverson-Blois.
# 2 samples total.
# error in 1st sample; 2nd sample is fine
# same issue as above. The Syverson-Blois bounds chron is repeated
# this is slightly different though. There are two bounds chrons, no event chron. Was one of them misnamed? 

i <- 3 # "Lost Chicken Creek"
# one site, one collunit, one dataset
# three chrons: 'Large scale', two Syverson-Blois.
# 10 samples
# m=6, same error as above, repeated chrons
# > sampObject
# An object of class "sample"
# Slot "ages":
#   age                         agetype ageolder ageyounger chronologyid         chronologyname
# 1    NA Calibrated radiocarbon years BP     8400       6300        49899 Syverson-Blois: bounds
# 2    NA Calibrated radiocarbon years BP    33712       8782        49899 Syverson-Blois: bounds
# 3  7400 Calibrated radiocarbon years BP     7600       7100        49900  Syverson-Blois: event
# 4 21287 Calibrated radiocarbon years BP    26217      16347        49900  Syverson-Blois: event

# m=7
# > sampObject
# An object of class "sample"
# Slot "ages":
#   age                         agetype ageolder ageyounger chronologyid         chronologyname
# 1    NA Calibrated radiocarbon years BP     8900       3400        49899 Syverson-Blois: bounds
# 2    NA Calibrated radiocarbon years BP    33712       8782        49899 Syverson-Blois: bounds
# 3  6200 Calibrated radiocarbon years BP     7200       5200        49900  Syverson-Blois: event
# 4 21287 Calibrated radiocarbon years BP    26217      16347        49900  Syverson-Blois: event

# m=8
# > sampObject
# An object of class "sample"
# Slot "ages":
#   age                         agetype ageolder ageyounger chronologyid         chronologyname
# 1    NA Calibrated radiocarbon years BP    10300       7400        49899 Syverson-Blois: bounds
# 2    NA Calibrated radiocarbon years BP    33712       8782        49899 Syverson-Blois: bounds
# 3  8800 Calibrated radiocarbon years BP     9300       8200        49900  Syverson-Blois: event
# 4 21287 Calibrated radiocarbon years BP    26217      16347        49900  Syverson-Blois: event

# m=9 worked, but it shouldn't
# > sampObject
# An object of class "sample"
# Slot "ages":
#     age                         agetype ageolder ageyounger chronologyid         chronologyname
# 1    NA Calibrated radiocarbon years BP    10300       7400        49899 Syverson-Blois: bounds
# 2    NA Calibrated radiocarbon years BP    33712       8782        49899 Syverson-Blois: bounds
# 3  8800 Calibrated radiocarbon years BP     9300       8200        49900  Syverson-Blois: event
# 4 21287 Calibrated radiocarbon years BP    26217      16347        49900  Syverson-Blois: event

# ok - this exposed an issue in the code. 
# I created a fix by 1) specifying there should only be one 'bounds' chron, not >0, then 2) adding a flag if it's encountered in other sites
# I should go back to the original code and re-run, to see if it affects other sites.  

for (i in 1:length(focalSites)){

  # pull out relevant site metadata
  sitename <- focalSites[[i]]@sitename
  site_coords <- NULL
  site_coords$long <- focalSites_coords[i,'long']
  site_coords$lat <- focalSites_coords[i,'lat']
  ids <- getids(focalSites[i])
  elev <- focalSites[[i]]@altitude
  
  ncollunits <- length(unique(ids$collunitid)) # this stores the number of collunits within the site
  
  for (j in 1:ncollunits){
    
    depositionalenvironment <- focalSites[[i]]$collunits[[j]]@depositionalenvironment # pull out more metadata, stored at collunit level
    sample_metadata <- cbind(sitename, ids[j, c('siteid', 'collunitid')], site_coords, elev, depositionalenvironment) # create metadata object for later appending
    
    chrons <- chronologies(focalSites[[i]]$collunits[j])
    chronDat <- do.call(rbind, lapply(chrons@chronologies, as.data.frame))
    
    ndatasets <- length(focalSites[[i]]@collunits[[j]]@datasets) # how many datasets are within the collunit?
    
    for (k in 1:ndatasets){
      nsamples <- length(focalSites[[i]]@collunits[[j]]@datasets[[k]]@samples) # how many samples are within the dataset?
      
      for (m in 1:nsamples){
        
        # find the vertebrate fauna data
        sampObject <- focalSites[[i]]@collunits[[j]]@datasets[[k]]@samples[[m]] # isolate a single sample
        
        if (length(which(sampObject@ages$chronologyname == "Syverson-Blois: bounds")) > 1){
          cat("site = ", i, "; sample m = ", m, "; More than one bounds chron", fill=T)
        }
        
        # only do the following if there is a new Syverson-Blois chronology. 
        if (length(which(sampObject@ages$chronologyname == "Syverson-Blois: bounds")) ==1){
          
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
          datasetid <- rep(focalSites[[i]]@collunits[[j]]@datasets[[k]]@datasetid, nrow(sampAges))
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

