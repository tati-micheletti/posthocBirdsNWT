retrieveRasters <- function(dataFolder,
                            years = c(2000, 2100), 
                            patternsToRetrieveRasters = NULL, 
                            species = NULL#, # Bird species names, for example
                              ){
  if (is.null(species)){
    warning("species is NULL. Defaulting to 'CAWA'", 
            immediate. = TRUE)
    species <- "CAWA"
  }
  if (is.null(patternsToRetrieveRasters)){
    warning("patternsToRetrieveRasters is NULL. All '.tif' files in each one of the the 
            dataFolders will be returned.", immediate. = TRUE)
    patternsToRetrieveRasters <- ".tif"
  }
  rastersOrganized <- lapply(X = names(dataFolder), function(eachFolder){
    runFiles <- lapply(X = names(dataFolder[[eachFolder]]), function(eachRun){
      allFiles <- grepMulti(x = list.files(path = dataFolder[[eachFolder]][[eachRun]], 
                                           full.names = TRUE), 
                            patterns = patternsToRetrieveRasters)
      groupFiles <- lapply(X = species, FUN = function(eachGroup){
        filesPath <- grepMulti(x = allFiles, patterns = c(eachGroup, paste(years, collapse = "|")))
        rastersTS <- raster::stack(lapply(X = years, FUN = function(eachTS){
          rasPath <- grepMulti(x = filesPath, patterns = eachTS)
          if (length(rasPath) == 0) 
            stop("At least one of the rasters doesn't seem to exist for the year sequence provided. Please check your data")
          ras <- raster::raster(rasPath)
          names(ras) <- paste(eachFolder, eachGroup, eachTS, sep = "_")
          return(ras)
        })
        )
        return(rastersTS)
      })
      names(groupFiles) <- species
      return(groupFiles)
    })
    names(runFiles) <- names(dataFolder[[eachFolder]])
    return(runFiles)
  })
  names(rastersOrganized) <- names(dataFolder)
 return(rastersOrganized)
}
