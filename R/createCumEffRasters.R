createCumEffRasters <- function(species = c("CAWA", "OSFL", "RUBL"),
                                rasFolder,
                                googlefolderID = NULL,
                                overwrite = FALSE){

  message(paste0("Calculating averaged cummulative effects rasters..."))
  
CEras <- future_lapply(species, function(SP){ 
    birdsPaths <- grepMulti(list.files(rasFolder, full.names = TRUE),
                            patterns = c(SP, "delta.tif"))
    spStack <- stack(lapply(birdsPaths, function(pths){
      B <- raster(pths)
      B[] <- B[]
      return(B)
    })
    )
    tic(paste0("Averaged cummulative effects rasters for ", SP))
    aveName <- file.path(rasFolder, paste0("averageDelta", SP, ".tif"))
    if (!file.exists(aveName)){
      averageSP <- calc(spStack, fun = mean)
      names(averageSP) <- paste0("averageDelta", SP, ".tif")
      writeRaster(averageSP, filename = aveName, format = "GTiff", overwrite = overwrite)
    }
    toc()
    
    tic(paste0("Deviation cummulative effects rasters for ", SP))
    sdName <- file.path(rasFolder, paste0("sdDelta", SP, ".tif"))
    if (!file.exists(sdName)){  
      sdSP <- calc(spStack, fun = sd)
      names(sdSP) <- paste0("sdDelta", SP, ".tif")
      writeRaster(sdSP, filename = sdName, format = "GTiff", overwrite = overwrite)
    }
    toc()

    if (!is.null(googlefolderID)){
      lapply(c(aveName, sdName), function(ras){
        drive_upload(ras, as_id(googlefolderID))
      })
    }
    return(list(averageCE = aveName, deviationCE = sdName))
  })
message(crayon::green(paste0("Rasters created for ", paste(species, collapse = ", "))))
  names(CEras) <- species
  return(CEras)
}
