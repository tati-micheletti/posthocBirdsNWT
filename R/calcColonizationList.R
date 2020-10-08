calcColonizationList <- function(listOfRasters, 
                                 species,
                                 years,
                                 comparisons,
                                 outputFolder,
                                 overwrite,
                                 useFuture,
                                 percentToDiscard = 0.3){
  
  outputFolder <- checkPath(file.path(outputFolder, "colonization"), create = TRUE)
  if (useFuture) plan("multiprocess", workers = length(species)/2)
  allBirds <- future_lapply(names(listOfRasters), function(sp){
    allScenarios <- lapply(names(listOfRasters[[sp]]), function(scenario){
      tic(paste0("Calculating colonization/extirpation for ", paste(sp, scenario, sep = " ")))
      allMods <- lapply(names(listOfRasters[[sp]][[scenario]]), function(bmod){
        allRuns <- lapply(names(listOfRasters[[sp]][[scenario]][[bmod]]), function(runs){
          colRasPath <- file.path(outputFolder, paste("colonization", sp, scenario, bmod, runs, sep = "_"))
          if (!file.exists(paste0(colRasPath, ".tif"))){
            rasStk <- listOfRasters[[sp]][[scenario]][[bmod]][[runs]]
            colRas <- calcColonization(rasT0Path = rasStk[[1]], 
                                       rasT1Path = rasStk[[nlayers(rasStk)]],
                                       rasName = colRasPath)
          }
          return(raster::raster(paste0(colRasPath, ".tif")))
        })
        names(allRuns) <- names(listOfRasters[[sp]][[scenario]][[bmod]])
        return(allRuns)
      })
      names(allMods) <- names(listOfRasters[[sp]][[scenario]])
      toc()
      return(allMods)
    })
    names(allScenarios) <- names(listOfRasters[[sp]])
    allScenariosUnlisted <- unlist(allScenarios)
    allComparisons <- names(allScenariosUnlisted)

    # Now I need to mix-match the ones that are the "replicates" (i.e. average the combinations that 
    # separate each one of the effects -- climate, fire, vegetation)
    factorialRasters <- lapply(names(comparisons), FUN = function(eachComparison){
      tic(paste0("Probability of colonization and extirpation calculated for ", eachComparison, 
                 " for ", sp))
      climateGroupNames <- sort(allComparisons[grep(comparisons[[eachComparison]][1], 
                                                    x = allComparisons)])
      nonclimateGroupNames <- sort(allComparisons[grep(comparisons[[eachComparison]][2], 
                                                       x = allComparisons)])
      
      ############### CLIMATE SENSITIVE
      climateGroup <- raster::stack(allScenariosUnlisted[names(allScenariosUnlisted) %in% climateGroupNames])
      propColonizationClimatePath <- file.path(outputFolder, paste("colonization", "climateSensitive", 
                                                               eachComparison, 
                                                               sp, sep = "_"))
      propExtirpationClimatePath <- file.path(outputFolder, paste("extirpation", "climateSensitive", 
                                                               eachComparison, 
                                                               sp, sep = "_"))
      if (!file.exists(paste0(propColonizationClimatePath, ".tif"))){
        probColClimateGroup <- calc(x = climateGroup, fun = function(x){.meanOfSpecificValue(x, val = 1)},
                                    filename = propColonizationClimatePath,
                                    format = "GTiff")
      } else probColClimateGroup <- raster::raster(paste0(propColonizationClimatePath, ".tif"))
      if (!file.exists(paste0(propExtirpationClimatePath, ".tif"))){
        probExtClimateGroup <- calc(x = climateGroup, fun = function(x){.meanOfSpecificValue(x, val = -1)},
                                    filename = propExtirpationClimatePath,
                                    format = "GTiff")
      } else probExtClimateGroup <- raster::raster(paste0(propExtirpationClimatePath, ".tif"))
      ############### NON CLIMATE SENSITIVE
      nonclimateGroup <- raster::stack(allScenariosUnlisted[names(allScenariosUnlisted) %in% nonclimateGroupNames])
      propColonizationNonClimatePath <- file.path(outputFolder, paste("colonization", "nonclimateSensitive", 
                                                                      eachComparison, 
                                                                      sp, sep = "_"))
      propExtirpationNonClimatePath <- file.path(outputFolder, paste("extirpation", "nonclimateSensitive", 
                                                                  eachComparison, 
                                                                  sp, sep = "_"))
      if (!file.exists(paste0(propColonizationNonClimatePath, ".tif"))){
        probColNonclimateGroup <- calc(x = nonclimateGroup, fun = function(x){.meanOfSpecificValue(x, val = 1)},
                                    filename = propColonizationNonClimatePath,
                                    format = "GTiff")
      } else probColNonclimateGroup <- raster::raster(paste0(propColonizationNonClimatePath, ".tif"))
      if (!file.exists(paste0(propExtirpationNonClimatePath, ".tif"))){
        probExtNonclimateGroup <- calc(x = nonclimateGroup, fun = function(x){.meanOfSpecificValue(x, val = -1)},
                                    filename = propExtirpationNonClimatePath,
                                    format = "GTiff")
      } else probExtNonclimateGroup <- raster::raster(paste0(propExtirpationNonClimatePath, ".tif"))
      toc()
      return(list(colonizationClimateSensitive = probColClimateGroup,
                  extirpationClimateSensitive =  probExtClimateGroup,
                  colonizationNonClimateSensitive = probColNonclimateGroup,
                  extirpationNonClimateSensitive = probExtNonclimateGroup))
    })
    names(factorialRasters) <- names(comparisons)
    
    
    # And finally the one for the net climate effect on birds
      tic(paste0("Probability of colonization and extirpation calculated for the net climate effect for ", sp))
      clim <- unlist(lapply(comparisons, `[[`, 1))
      nonclim <- unlist(lapply(comparisons, `[[`, 2))
      climateGroupNames <- sort(grepMulti(allComparisons, patterns = clim))
      nonclimateGroupNames <- sort(grepMulti(allComparisons, patterns = nonclim))
      
      ############### CLIMATE SENSITIVE
      climateGroup <- raster::stack(allScenariosUnlisted[names(allScenariosUnlisted) %in% climateGroupNames])
      propColonizationClimatePath <- file.path(outputFolder, paste("colonization", "climateSensitive", 
                                                                   "netEffect", 
                                                                   sp, sep = "_"))
      propExtirpationClimatePath <- file.path(outputFolder, paste("extirpation", "climateSensitive", 
                                                                  "netEffect", 
                                                                  sp, sep = "_"))
      if (!file.exists(paste0(propColonizationClimatePath, ".tif"))){
        probColClimateGroup <- calc(x = climateGroup, fun = function(x){.meanOfSpecificValue(x, val = 1)},
                                    filename = propColonizationClimatePath,
                                    format = "GTiff")
      } else probColClimateGroup <- raster::raster(paste0(propColonizationClimatePath, ".tif"))
      if (!file.exists(paste0(propExtirpationClimatePath, ".tif"))){
        probExtClimateGroup <- calc(x = climateGroup, fun = function(x){.meanOfSpecificValue(x, val = -1)},
                                    filename = propExtirpationClimatePath,
                                    format = "GTiff")
      } else probExtClimateGroup <- raster::raster(paste0(propExtirpationClimatePath, ".tif"))
      ############### NON CLIMATE SENSITIVE
      nonclimateGroup <- raster::stack(allScenariosUnlisted[names(allScenariosUnlisted) %in% nonclimateGroupNames])
      propColonizationNonClimatePath <- file.path(outputFolder, paste("colonization", "nonclimateSensitive", 
                                                                      "netEffect", 
                                                                      sp, sep = "_"))
      propExtirpationNonClimatePath <- file.path(outputFolder, paste("extirpation", "nonclimateSensitive", 
                                                                     "netEffect", 
                                                                     sp, sep = "_"))
      if (!file.exists(paste0(propColonizationNonClimatePath, ".tif"))){
        probColNonclimateGroup <- calc(x = nonclimateGroup, fun = function(x){.meanOfSpecificValue(x, val = 1)},
                                       filename = propColonizationNonClimatePath,
                                       format = "GTiff")
      } else probColNonclimateGroup <- raster::raster(paste0(propColonizationNonClimatePath, ".tif"))
      if (!file.exists(paste0(propExtirpationNonClimatePath, ".tif"))){
        probExtNonclimateGroup <- calc(x = nonclimateGroup, fun = function(x){.meanOfSpecificValue(x, val = -1)},
                                       filename = propExtirpationNonClimatePath,
                                       format = "GTiff")
      } else probExtNonclimateGroup <- raster::raster(paste0(propExtirpationNonClimatePath, ".tif"))
      toc()
      
      netEffect <- list(colonizationClimateSensitive = probColClimateGroup,
                        extirpationClimateSensitive =  probExtClimateGroup,
                        colonizationNonClimateSensitive = probColNonclimateGroup,
                        extirpationNonClimateSensitive = probExtNonclimateGroup)
      factorialRasters <- c(factorialRasters, netEffect = list(netEffect))
      return(factorialRasters)
    })
  plan("sequential")
  names(allBirds) <- names(listOfRasters)
  return(allBirds)
}

calcColonization <- function(rasT0Path, rasT1Path, 
                             percentToDiscard = 0.3, rasName){
  if (is(rasT0Path, "character")){
    rasT0Path <- raster::raster(rasT0Path)
  }
  if (is(rasT1Path, "character")){
    rasT1Path <- raster::raster(rasT1Path)
  }
  rasT0 <- .presenceAbsenceRas(rasT0Path, percentToDiscard)
  rasT1 <- .presenceAbsenceRas(rasT1Path, percentToDiscard)
  rasCol <- rasT1 - rasT0
  writeRaster(rasCol, filename = rasName, format = "GTiff")
  return(raster::raster(paste0(rasName, ".tif")))
}

.presenceAbsenceRas <- function(ras, percentToDiscard = 0.3){
  CSdt <- data.table::data.table(pixelID = 1:ncell(ras),
                                 val = getValues(ras))
  CSdt <- na.omit(CSdt)
  data.table::setkey(CSdt, val)
  CSdt[, CUM := cumsum(val)]
  CSdt[, CUMstd := CUM/sum(val)]
  CSdt[, PA := CUMstd > percentToDiscard]
  BIRDpres <- raster(ras)
  BIRDpres[CSdt[PA == TRUE, pixelID]] <- 1
  BIRDpres[CSdt[PA == FALSE, pixelID]] <- 0
  return(BIRDpres)
}

.meanOfSpecificValue <- function(x, val) {
  return(sum(x == val)/length(x))
}
