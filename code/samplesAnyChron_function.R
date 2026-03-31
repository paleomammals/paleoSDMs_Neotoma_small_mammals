# Neotoma samples function. This returns samples associated with every available chronology.

# Input variables:
  # x is equivalent to the saved object output from a call to 'get_downloads'
  # chronName is the exact name of the chronology you wish to choose, equivalent to chronologyname

samplesAnyChron <- function(x, chronName){ 

  # create blank samples list for the dataset   
  datasetSamples <- data.frame() 

  # set the coordinates file:
  x_coords <- coordinates(x)
  
  for (i in 1:length(x)){
    
    # pull out relevant site metadata
    sitename <- x[[i]]@sitename
    siteid <- x[[i]]@siteid
    long <- x_coords[i,'long']
    lat <- x_coords[i,'lat']
    elev <- x[[i]]@altitude
    
    ids <- getids(x[i]) # retrieve all ids associated with the site
    ncollunits <- length(unique(ids$collunitid)) # this stores the number of collunits within the site
    
    for (j in 1:ncollunits){
      
      depositionalenvironment <- x[[i]]$collunits[[j]]@depositionalenvironment # pull out more metadata, stored at collunit level
      collunitid <- x[[i]]$collunits[[j]]@collectionunitid
      handle <- x[[i]]$collunits[[j]]@handle
      sample_metadata <- as.data.frame(cbind(siteid, sitename, collunitid, handle, long, lat, elev, depositionalenvironment)) # create metadata object for later appending
      
      chrons <- chronologies(x[[i]]$collunits[j])
      chronDat <- do.call(rbind, lapply(chrons@chronologies, as.data.frame))
      
      ndatasets <- length(x[[i]]@collunits[[j]]@datasets) # how many datasets are within the collunit?
      
      for (k in 1:ndatasets){
        nsamples <- length(x[[i]]@collunits[[j]]@datasets[[k]]@samples) # how many samples are within the dataset?
        
        for (m in 1:nsamples){
          
          # find the vertebrate fauna data
          sampObject <- x[[i]]@collunits[[j]]@datasets[[k]]@samples[[m]] # isolate a single sample
          
          # only do the following if there is only one chronology that corresponds to chronName. 
          if (length(which(sampObject@ages$chronologyname == chronName)) ==1){
            
            sampDF <- do.call(cbind, lapply(sampObject@datum, as.data.frame)) # convert the NISP/MNI/PA data to a table
            colnames(sampDF) <- names(sampObject@datum) # rename the dataframe
            
            agesTemp <- sampObject@ages[which(sampObject@ages$chronologyname == chronName),] # pull out only those samples with a Syverson-Blois chron
            
            sampAges <- as.data.frame(matrix(nrow=nrow(sampDF), ncol=ncol(agesTemp)))
            colnames(sampAges) <- colnames(agesTemp)
            sampAges[,1:ncol(sampAges)] <- agesTemp
            
            # add on higher level metadata
            sample_metadata_repeated <- as.data.frame(matrix(nrow=nrow(sampDF), ncol=ncol(sample_metadata)))
            colnames(sample_metadata_repeated) <- colnames(sample_metadata)
            sample_metadata_repeated[,1:ncol(sample_metadata)] <- sample_metadata
            
            # Store some extra ids and names
            datasetid <- rep(x[[i]]@collunits[[j]]@datasets[[k]]@datasetid, nrow(sampAges))
            analysisunitid <- rep(sampObject@analysisunitid, nrow(sampAges))
            analysisunitname <- rep(sampObject@analysisunitname, nrow(sampAges))
            sample_metadata_repeated$datasetid <- datasetid
            sample_metadata_repeated$analysisunitid <- analysisunitid
            sample_metadata_repeated$analysisunitname <- analysisunitname
            
            # merge the three dataframes together
            sampleDF_full <- bind_cols(sample_metadata_repeated, sampDF, sampAges)
            
            # bind this specific dataset to the full dataset list
            datasetSamples <- bind_rows(datasetSamples, sampleDF_full)
            cat("i = ", i, "samples = ", nrow(sampleDF_full), "; cumul. samples = ", nrow(datasetSamples), fill=T) # track progress, mostly useful for testing with smaller number of sites
            
          } # close if statement for chronName
        } # close m (samples)
      } # close k (datasets)
    } # close j (collunits)
  } # close i (sites)
  
  datasetSamples
}