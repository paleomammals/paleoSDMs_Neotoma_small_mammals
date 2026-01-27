### OLD CODE ----




# if there is a 1:1 match between collunits and datasets (otherwise the loop breaks on the chronologies or samples for some sites)
if (ncollunits == ndatasets){
  
  for (j in 1:nrow(ids)){
    
    chrons <- chronologies(vertDownloads[[i]]$collunits[j])
    chronDat <- do.call(rbind, lapply(chrons@chronologies, as.data.frame))
    
    if (any(which(chronDat$chronologyname == "Syverson-Blois: bounds"))) {
      
      df_samples <- data.frame() # create taxon list across all chronologies
      for (k in seq_along(chrons)) {
        vertDownloads[[i]]$collunits[[j]]$chronologies <- NULL
        vertDownloads[[i]]$collunits[[j]]$chronologies <- set_default(new("chronologies", chronologies = list(chrons[[k]])),
                                                                      chrons[[k]]$chronologyid)
        df <- samples(vertDownloads[[i]]$collunits[[j]])
        df_samples <- bind_rows(df_samples, df)
      }
      
      # filter samples to be just those from the correct chronology and remove samples from the wrong chronology
      if (!all(df_samples$chronologyname == "Syverson-Blois: bounds")) { # some sites have FAUNMAP as default as well as Syverson-Blois
        df_samples <- df_samples[which(df_samples$chronologyname == "Syverson-Blois: bounds"),]
      }
      
      # add on the download metadata
      sample_metadata_repeated <- as.data.frame(matrix(nrow=nrow(df_samples), ncol=ncol(sample_metadata)))
      colnames(sample_metadata_repeated) <- colnames(sample_metadata)
      sample_metadata_repeated[,1:ncol(sample_metadata)] <- sample_metadata
      
      df_samples_full <- bind_cols(sample_metadata_repeated, df_samples)
      samples_keep <- bind_rows(samples_keep, df_samples_full)
      #rm(sample_metadata)
    }
  }
}else{
  print(cat(i, " - Site skipped"))
  skipped <- c(skipped, i)
}
}




# # EXAMPLE CODE
# sts <- get_sites(c(3673, 3545))
# sts_d <- get_datasets(sts)
# dls <- get_downloads(sts)
# dls
# 
# site_coords$long <- dls@sites[[2]]@geography$geometry[[1]][1]
# site_coords$lat <- dls@sites[[2]]@geography$geometry[[1]][2]
# site_coords
# 
# site_coords$long <- dls@sites[[1]]@geography$geometry[[1]][1]
# site_coords$lat <- dls@sites[[1]]@geography$geometry[[1]][2]
# site_coords


sts <- get_sites(3568)
dls <- get_downloads(sts)
i <- j <- 1

chrons <- chronologies(dls[[i]]$collunits[j])
chronDat <- do.call(rbind, lapply(chrons@chronologies, as.data.frame))

if (any(which(chronDat$chronologyname == "Syverson-Blois: bounds"))) {
  
  defaultChronID <- chronDat$chronologyid[which(chronDat$chronologyname == "Syverson-Blois: bounds")] # which index should be default?
  dls[[i]]$collunits[[j]]$chronologies <- set_default(dls[[i]]$collunits[[j]]$chronologies, defaultChronID)
  
  # check defaults:
  do.call(rbind, lapply(chrons@chronologies, as.data.frame)) # looks good
  
  df_samples <- samples(dls[[i]]$collunits[[j]]) # create taxon list
  any(df_samples$chronologyname == "Syverson-Blois: bounds", na.rm=T)
  
  # can I change the FAUNMAP default to be FALSE? Not yet
  faunmap_chron <- which(chronDat$chronologyname == "FAUNMAP 1.1")
  dls[[i]]$collunits[[j]]$chronologies[[faunmap_chron]]$isdefault # this returns TRUE - can I reset to FALSE?
  dls[[i]]$collunits[[j]]$chronologies[[faunmap_chron]]$isdefault <- "FALSE" # returns an error
  dls[[i]]$collunits[[j]]$chronologies[[faunmap_chron]]$isdefault <- FALSE # also returns an error
  
}


dfs <- data.frame()
for (k in seq_along(chrons)) {
  dls[[i]]$collunits[[j]]$chronologies <- NULL
  dls[[i]]$collunits[[j]]$chronologies <- set_default(new("chronologies", chronologies = list(chrons[[k]])),
                                                      chrons[[k]]$chronologyid)
  df <- samples(dls[[i]])
  dfs <- bind_rows(dfs, df)
}

#chrons@chronologies[[2]]@chronologyname
for (s in seq_along(pollen_dl)){
  chs <- chronologies(pollen_dl[[s]])
  for (i in seq_along(chs)) {
    pollen_dl[[s]]$collunits[[1]]$chronologies <- NULL
    pollen_dl[[s]]$collunits[[1]]$chronologies <- set_default(new("chronologies", chronologies = list(chs[[i]])),
                                                              chs[[i]]$chronologyid)
    df <- samples(pollen_dl[[s]])
  }
  dfs <- bind_rows(dfs, df)
}



# # find all datasets that have a new Syverson Blois bounds chronology
# df_keep <- data.frame() 
# skipped <- NULL
# 
# for (i in 1:length(vertDownloads)){
#   ids<- getids(vertDownloads[i])
#   ncollunits <- length(unique(ids$collunitid))
#   ndatasets <- length(unique(ids$datasetid))
#   
#   # if there is a 1:1 match between collunits and datasets
#   if (ncollunits == ndatasets){
#     for (j in 1:nrow(ids)){
#       
#       chrons <- chronologies(vertDownloads[[i]]$collunits[j])
#       #chrons <- chronologies(vertDownloads[[i]]$collunits[1]$datasets[1])
#       chronDat <- do.call(rbind, lapply(chrons@chronologies, as.data.frame))
#       
#       if (any(which(chronDat$chronologyname == "Syverson-Blois: bounds"))) {
#         keepDownloadIndex <- i #which sites do we want to keep?
#         keepDatasetID <-  ids$datasetid[j]
#         defaultChronID <- chronDat$chronologyid[which(chronDat$chronologyname == "Syverson-Blois: bounds")] # which index should be default?
#         dfs_keep <- data.frame(index = keepDownloadIndex, datasetID = keepDatasetID, defaultChronID = defaultChronID)
#         df_keep <- bind_rows(df_keep, dfs_keep)
#       }
#     }
#   }else{
#     print(cat(i, " - Site skipped"))
#     skipped <- c(skipped, i)
#   }
# }
# 
# # trim datasets to those with updated chron
# vertDownloads_keep <- vertDownloads[df_keep$index]
# 
# # now find the fauna in each dataset
# dfs <- data.frame()
# for (s in seq_along(vertDownloads_keep)){
#   ids<- getids(vertDownloads[i])
#   ncollunits <- length(unique(ids$collunitid))
#   ndatasets <- length(unique(ids$datasetid))
#   
#   # if there is a 1:1 match between collunits and datasets
#   if (ncollunits == ndatasets){
#     for (j in 1:nrow(ids)){
#       vertDownloads_keep[[s]]$collunits[[j]]$chronologies <- set_default(vertDownloads_keep[[s]]$collunits[[j]]$chronologies, df_keep$defaultChronID[s])
#       df <- samples(vertDownloads_keep[[s]])
#       dfs <- bind_rows(dfs, df)
# }
# 
#     



sts <- get_sites(3687)
dls <- get_downloads(sts)
sts
length(chronologies(dls))

ex1 <- vertDownloads[[161]]
ex1
length(chronologies(ex1))

i <- j <- 1 

sts2 <- get_datasets(siteid=3687)
dls2 <- get_downloads(sts2)

chronologies(dls)
samples(dls)


# vertDownloads[[1]]@collunits[[j]]@datasets[[1]]@samples[[1]]@ages




# find all datasets that have a new Syverson Blois bounds chronology
samples_keep <- data.frame() 
skipped <- NULL

for (i in 1:length(vertDownloads)){
  
  # pull out site metadata
  sitename <- vertDownloads[[i]]@sitename
  site_coords <- NULL
  site_coords$long <- vertDownloads_coords[i,'long']
  site_coords$lat <- vertDownloads_coords[i,'lat']
  ids <- getids(vertDownloads[i])
  elev <- vertDownloads[[i]]@altitude
  
  ncollunits <- length(unique(ids$collunitid))
  ndatasets <- length(unique(ids$datasetid))
  
  # if there is a 1:1 match between collunits and datasets (otherwise the loop breaks on the chronologies or samples for some sites)
  if (ncollunits == ndatasets){
    
    for (j in 1:nrow(ids)){
      depositionalenvironment <- vertDownloads[[i]]$collunits[[j]]@depositionalenvironment
      sample_metadata <- cbind(sitename, ids[j, c('siteid', 'collunitid')], site_coords, elev, depositionalenvironment)
      
      chrons <- chronologies(vertDownloads[[i]]$collunits[j])
      chronDat <- do.call(rbind, lapply(chrons@chronologies, as.data.frame))
      
      if (any(which(chronDat$chronologyname == "Syverson-Blois: bounds"))) {
        
        df_samples <- data.frame() # create taxon list across all chronologies
        for (k in seq_along(chrons)) {
          vertDownloads[[i]]$collunits[[j]]$chronologies <- NULL
          vertDownloads[[i]]$collunits[[j]]$chronologies <- set_default(new("chronologies", chronologies = list(chrons[[k]])),
                                                                        chrons[[k]]$chronologyid)
          df <- samples(vertDownloads[[i]]$collunits[[j]])
          df_samples <- bind_rows(df_samples, df)
        }
        
        # filter samples to be just those from the correct chronology and remove samples from the wrong chronology
        if (!all(df_samples$chronologyname == "Syverson-Blois: bounds")) { # some sites have FAUNMAP as default as well as Syverson-Blois
          df_samples <- df_samples[which(df_samples$chronologyname == "Syverson-Blois: bounds"),]
        }
        
        # add on the download metadata
        sample_metadata_repeated <- as.data.frame(matrix(nrow=nrow(df_samples), ncol=ncol(sample_metadata)))
        colnames(sample_metadata_repeated) <- colnames(sample_metadata)
        sample_metadata_repeated[,1:ncol(sample_metadata)] <- sample_metadata
        
        df_samples_full <- bind_cols(sample_metadata_repeated, df_samples)
        samples_keep <- bind_rows(samples_keep, df_samples_full)
        #rm(sample_metadata)
      }
    }
  }else{
    print(cat(i, " - Site skipped"))
    skipped <- c(skipped, i)
  }
}

### deal with skipped sites ---

vertDownloads_skipped_coords <- coordinates(vertDownloads_skipped)

samples_keep <- data.frame() 
for (i in 1:length(vertDownloads_skipped)){
  
  # pull out site metadata
  sitename <- vertDownloads_skipped[[i]]@sitename
  site_coords <- NULL
  site_coords$long <- vertDownloads_skipped_coords[i,'long']
  site_coords$lat <- vertDownloads_skipped_coords[i,'lat']
  ids <- getids(vertDownloads_skipped[i])
  elev <- vertDownloads_skipped[[i]]@altitude
  
  ncollunits <- length(unique(ids$collunitid))
  ndatasets <- length(unique(ids$datasetid))
  
  # if there is a 1:1 match between collunits and datasets (otherwise the loop breaks on the chronologies or samples for some sites)
  if (ncollunits != ndatasets){
    for (j in 1:nrow(ids)){
      depositionalenvironment <- vertDownloads_skipped[[i]]$collunits[[j]]@depositionalenvironment
      sample_metadata <- cbind(sitename, ids[j, c('siteid', 'collunitid')], site_coords, elev, depositionalenvironment)
      
      chrons <- chronologies(vertDownloads_skipped[[i]]$collunits[j])
      chronDat <- do.call(rbind, lapply(chrons@chronologies, as.data.frame))
      
      if (any(which(chronDat$chronologyname == "Syverson-Blois: bounds"))) {
        
        defaultChronID <- chronDat$chronologyid[which(chronDat$chronologyname == "Syverson-Blois: bounds")] # which index should be default?
        vertDownloads_skipped[[i]]$collunits[[j]]$chronologies <- set_default(vertDownloads_skipped[[i]]$collunits[[j]]$chronologies, defaultChronID)
        
        df_samples <- samples(vertDownloads_skipped[[i]]$collunits[[j]]) # create taxon list
        
        if (!all(df_samples$chronologyname == "Syverson-Blois: bounds")) { # some sites have FAUNMAP as default as well as Syverson-Blois
          df_samples <- df_samples[which(df_samples$chronologyname == "Syverson-Blois: bounds"),]
        }
        
        # add on the download metadata
        sample_metadata_repeated <- as.data.frame(matrix(nrow=nrow(df_samples), ncol=ncol(sample_metadata)))
        colnames(sample_metadata_repeated) <- colnames(sample_metadata)
        sample_metadata_repeated[,1:ncol(sample_metadata)] <- sample_metadata
        
        df_samples_full <- bind_cols(sample_metadata_repeated, df_samples)
        samples_keep <- bind_rows(samples_keep, df_samples_full)
      }
    }
  }
}

# create a new download object
vertDownloads_skipped <- vertDownloads[skipped] # for all of these, the # of datasets does not equal # collection units.
# also need to deal with: vertDownloads_orig[c(1418, 2160)]
