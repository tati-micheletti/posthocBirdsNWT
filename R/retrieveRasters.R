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
   rastersOrganized <- lapply(X = species, function(sp){
     scenFiles <- lapply(X = names(dataFolder), function(eachScenario){
       whichSP <- which(species == sp)
      whichSCENARIO <- which(names(dataFolder) == eachScenario)
       totalCompleted <- 100*((whichSP*whichSCENARIO)/(length(species)*length(names(dataFolder))))
         message(paste0("Retrieving rasters for ", sp, " for ", eachScenario, ". COMPLETED: ",
                        totalCompleted, "%"))
       birdModels <- lapply(X = names(dataFolder[[eachScenario]]), function(bmod){
         runModels <- lapply(X = names(dataFolder[[eachScenario]][[bmod]]), function(run){  
            allFiles <- grepMulti(x = list.files(path = dataFolder[[eachScenario]][[bmod]][[run]],
                                                 full.names = TRUE),
                                  patterns = patternsToRetrieveRasters)
         filesPath <- grepMulti(x = allFiles, patterns = c(sp, paste(years, collapse = "|")))
         rastersTS <- raster::stack(lapply(X = years, FUN = function(eachTS){
           rasPath <- grepMulti(x = filesPath, patterns = eachTS)
           if (length(rasPath) == 0)
             stop("At least one of the rasters doesn't seem to exist for the year sequence provided. Please check your data")
           ras <- raster::raster(rasPath)
           names(ras) <- paste(eachScenario, bmod, run, eachTS, sep = "_")
           return(ras)
         })
         )
         return(rastersTS)
       })
       names(runModels) <- names(dataFolder[[eachScenario]][[bmod]])
       return(runModels)
     })
     names(birdModels) <- names(dataFolder[[eachScenario]])
     return(birdModels)
   })
   names(scenFiles) <- names(dataFolder)
  return(scenFiles)
   })
   names(rastersOrganized) <- species
   return(rastersOrganized)
  
  
 #  rastersOrganized <- lapply(X = names(dataFolder), function(eachFolder){
 #    runFiles <- lapply(X = names(dataFolder[[eachFolder]]), function(eachRun){
 #      birdModels <- lapply(X = names(dataFolder[[eachFolder]][[eachRun]]), function(bmod){
 #        allFiles <- grepMulti(x = list.files(path = dataFolder[[eachFolder]][[eachRun]][[bmod]], 
 #                                           full.names = TRUE), 
 #                            patterns = patternsToRetrieveRasters)
 #      groupFiles <- lapply(X = species, FUN = function(eachGroup){
 #        filesPath <- grepMulti(x = allFiles, patterns = c(eachGroup, paste(years, collapse = "|")))
 #        rastersTS <- raster::stack(lapply(X = years, FUN = function(eachTS){
 #          rasPath <- grepMulti(x = filesPath, patterns = eachTS)
 #          if (length(rasPath) == 0) 
 #            stop("At least one of the rasters doesn't seem to exist for the year sequence provided. Please check your data")
 #          ras <- raster::raster(rasPath)
 #          names(ras) <- paste(eachFolder, eachGroup, eachTS, sep = "_")
 #          return(ras)
 #        })
 #        )
 #        return(rastersTS)
 #      })
 #      names(groupFiles) <- species
 #      return(groupFiles)
 #    })
 #    names(birdModels) <- names(dataFolder[[eachFolder]][[eachRun]])
 #    return(birdModels)
 #  })
 #  names(runFiles) <- names(dataFolder[[eachFolder]])
 # return(runFiles)
 #  })
 #  names(rastersOrganized) <- names(dataFolder)
 #  return(rastersOrganized)
}
