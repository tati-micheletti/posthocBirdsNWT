makeDeltaRasters <- function(listOfRasters, 
                             species,
                             relativeDelta = TRUE, 
                             years = c(2000, 2100), 
                             outputFolder, lightLoad = TRUE,
                             overwrite = FALSE,
                             upload = FALSE,
                             folderID = NULL,
                             email = NULL,
                             useFuture = FALSE){
  
  rastersOrganized <- lapply(X = names(listOfRasters), function(eachSimulation){
    runFiles <- lapply(X = names(listOfRasters[[eachSimulation]]), FUN = function(eachRun){
      bmodFiles <- lapply(X = names(listOfRasters[[eachSimulation]][[eachRun]]), FUN = function(eachBirdMod){
        if (useFuture) plan("multiprocess", workers = length(species)/2)
      speciesFiles <- future_lapply(X = names(listOfRasters[[eachSimulation]][[eachRun]][[eachBirdMod]]),
                                    FUN = function(eachSpecies){ ########### future_lapply <~~~~~~~~~~~~~~~~~~~~
      currentGroupsRas <- listOfRasters[[eachSimulation]][[eachRun]][[eachBirdMod]][[eachSpecies]]
      firstRas <- currentGroupsRas[[1]]
      lastRas <- currentGroupsRas[[nlayers(currentGroupsRas)]]
      if (isTRUE(relativeDelta)){
        denom <- firstRas 
      } else {
        denom <- 1
      }
      rasName <- file.path(outputFolder, 
                           paste0(paste(eachSimulation, eachRun, eachBirdMod, eachSpecies, sep = "_"), 
                                  "delta.tif"))
      if (all(file.exists(rasName), !isTRUE(overwrite))){
        message(crayon::green(paste0(
          "Delta maps exist for ", eachSpecies, " for ", eachRun, " for ", eachBirdMod,
          " for ", eachSimulation, ". Returning"
          ))) 
      } else {
        tic(paste0("Calculating delta maps for ", eachSpecies, " for ", 
                                      eachRun, " for ", eachSimulation))
        deltaRas <- (lastRas - firstRas)/denom
        writeRaster(x = deltaRas, filename = rasName, overwrite = TRUE, format = "GTiff")
        rm(deltaRas)
        gc()
        toc()
      }
      if(upload){
        if (is.null(folderID)) stop("Please provide folderID when upload == TRUE")
        # Need to find which name of the folderID is contained in the `eachSimulation` as simulations can 
        # have bird model version, which is not identical to the folderID 
        # ie. eachSimulation = "LandR.CS_SCFM_V6" while names(folderID)[2] = "LandR.CS_SCFM"
        simulFolder <- names(folderID)[which(!is.na(pmatch(names(folderID), eachSimulation)))]
        googledrive::drive_auth(email)
        googledrive::drive_upload(rasName, path = googledrive::as_id(folderID[[simulFolder]]))
      }
      if (lightLoad){
        return(rasName)
      } else{
        return(raster::raster(rasName))
      }
      })
      plan("sequential")
      names(speciesFiles) <- names(listOfRasters[[eachSimulation]][[eachRun]][[eachBirdMod]])
      return(speciesFiles)
    })
      names(bmodFiles) <- names(listOfRasters[[eachSimulation]][[eachRun]])
      return(bmodFiles)      
    })
    names(runFiles) <- names(listOfRasters[[eachSimulation]])
    return(runFiles)
    })
    names(rastersOrganized) <- names(listOfRasters)
    return(rastersOrganized)
}