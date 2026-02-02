# This script is for retrieving and arranging small mammal data from Neotoma, for the paleoSDMs project
# Script develope by Jessica Blois

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
# There are six problematic sites. The script will not break on these sites, 
# but they have some issues with repeated or potentially missing chronologies.
# See the script 'ProblematicSites.R' for more details.
# I have re-run the script (2/2/2026) with a fix that skips the samples that don't have only one bounds chron

vertDownloads_coords <- coordinates(vertDownloads)

# download data sample by sample
# find all datasets that have a new Syverson Blois bounds chronology

datasetSamples <- data.frame() # create blank samples list for the dataset

for (i in 1:length(vertDownloads)){
  
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
        
        # this flags a site if there is more than one Bounds chronology
        if (length(which(sampObject@ages$chronologyname == "Syverson-Blois: bounds")) > 1){ 
          cat("site = ", i, "; sample m = ", m, "; More than one bounds chron", fill=T)
        }
        
        # only do the following if there is exactly one Syverson-Blois bounds chronology for a sample. 
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



### Clean up data ----

# relevant columns are: 
# siteid, sitename, datasetid, collunitid, lat, long, elev, ageolder, ageyounger, variablename, units, value, ecologicalgroup
# also filter to just keep the rodents and soricomorphs

dfs_simple <- datasetSamples %>% 
  select(siteid, sitename, datasetid, collunitid, lat, long, elev, ageolder, ageyounger, chronologyname, ecologicalgroup, variablename, units, value) %>%
  filter(ecologicalgroup == "RODE" | ecologicalgroup == "SORI")

# now, for each site, simplify to a presence-absence. In some cases, this means coarsening the MNI or NISP data to pr/ab
dfs_unique <- dfs_simple %>%
  distinct(siteid, sitename, datasetid, collunitid, lat, long, elev, ageolder, ageyounger, chronologyname, variablename)


# ok, this is the semi-final list of occurrences. now, you need to clean up the names

# 2. match taxa to the cleaned names ----

# import taxonomy file
# Note, this taxonomy file was develope as part of the broader Neotoma vertebrate taxonomy working group, and is in development
taxonDat <- read.csv("data/neotoma_taxa_matched.csv", header=T)

# do all names have a match?
all(match(dfs_unique$variablename, taxonDat$taxonname)) # should be TRUE

dfs_taxa <- taxonDat[match(dfs_unique$variablename, taxonDat$taxonname),]

# bind relevant taxonomic information onto dfs_unique

dfs_unique_clean <- cbind(dfs_unique, 
                          dfs_taxa[,c('taxonname_updated', 'MDD_species', 'order', 'family', 'genus', 'specificEpithet')])
                          
# filter out all taxa that are not identified to at least genus
dfs_unique_clean <- dfs_unique_clean %>%
  filter(!is.na(genus)) 

write.csv(dfs_unique_clean, file = "output/Neotoma_small_mammals_cleaned.csv", row.names=F)





