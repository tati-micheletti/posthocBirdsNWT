calcColonizationList_old <- function(listOfRasters, 
                                 species,
                                 years,
                                 comparisons,
                                 outputFolder,
                                 overwrite,
                                 useFuture){
  
  outputFolder <- checkPath(file.path(outputFolder, "colonization"), create = TRUE)

  allBirds <- lapply(names(listOfRasters), function(sp){
    allScenarios <- lapply(names(listOfRasters[[sp]]), function(scenario){
      allMods <- lapply(names(listOfRasters[[sp]][[scenario]]), function(bmod){
        tic(paste0("Calculating colonization for ", paste(sp, scenario, bmod, sep = " ")))
        runsList <- listOfRasters[[sp]][[scenario]][[bmod]]
        rearrangedRuns <- lapply(runsList, function(run){
          stk <- raster::stack(run[[1]], run[[nlayers(run)]]) 
          return(stk)
        })
        names(rearrangedRuns) <- names(runsList)
        averageRuns <- lapply(X = 1:2, FUN = function(index){
          sbset <- unlist(lapply(rearrangedRuns, `[[`, index), use.names = FALSE)
          averagePath <- file.path(outputFolder, paste(sp, 
                                                       scenario,
                                                       bmod,
                                                       ifelse(index == 1,
                                                              years[1],
                                                              years[length(years)]),
                                                       "average", 
                                                                       sep = "_"))
          stdDevPath <- file.path(outputFolder, paste(sp, scenario, bmod,
                                                      ifelse(index == 1, years[1],
                                                             years[length(years)]),
                                                                       "stdDev", 
                                                                       sep = "_"))
          if (file.exists(paste0(averagePath, ".tif"))){
            average <- raster::raster(paste0(averagePath, ".tif"))
          } else {
            sbset <- raster::stack(sbset)
            average <- calc(x = sbset, fun = mean, filename = averagePath, format = "GTiff")
                # time * 64 * 2 * 2 * 4 * 47 seconds = 14hs
          }
          if (file.exists(paste0(stdDevPath, ".tif"))){
            stdDev <- raster::raster(paste0(stdDevPath, ".tif"))
          } else {
            # stdDev <- calc(x = sbset, fun = function(x){sqrt(var(x))}, filename = stdDevPath, format = "GTiff")
                # time * 64 * 2 * 2 * 4 * 354 seconds = 4.2 days
            stdDev <- NULL
          }
          return(list(average = average, stdDev = stdDev))
        })
        names(averageRuns) <- paste0("Year", c(years[1], years[length(years)]))
        browser()
        # At this point we have 8 
        
        
        # We need to return 2 things: 
        # 1) the difference between 2011 and 2100; 
        diffRunsYearPath <- file.path(outputFolder, paste("diff_allRunsYears", sp, scenario, bmod, sep = "_"))
        if (file.exists(paste0(diffRunsYearPath, ".tif"))){
          diffYear <- raster::raster(paste0(diffRunsYearPath, ".tif"))
        } else {
          diffYear <- averageRuns[[paste0("Year", years[length(years)])]][["average"]] - 
            averageRuns[[paste0("Year", years[1])]][["average"]]
          names(diffYear) <- basename(diffRunsYearPath)
          writeRaster(diffYear, filename = diffRunsYearPath, format = "GTiff")
          diffYear <- raster::raster(paste0(diffRunsYearPath, ".tif"))
        }
        toc()
        return(diffYear)
        })
      names(allMods) <- names(listOfRasters[[sp]][[scenario]])
      return(allMods)
    })
    names(allScenarios) <- names(listOfRasters[[sp]])    
    # This one will take about 64 * 8min = 9hs
    allScenariosUnlisted <- unlist(allScenarios)
    allComparisons <- names(allScenariosUnlisted)

    # Now I need to mix-match the ones that are the "replicates" (i.e. average the combinations that 
    # separate each one of the effects -- climate, fire, vegetation)
    factorialRasters <- lapply(names(comparisons), FUN = function(eachComparison){
      comparison <- comparisons[[eachComparison]]
        firstGroupNames <- sort(allComparisons[grep(comparison[1], x = allComparisons)])
        secondGroupNames <- sort(allComparisons[grep(comparison[2], x = allComparisons)])
        firstGroup <- allScenariosUnlisted[names(allScenariosUnlisted) %in% firstGroupNames]
        secondGroup <- allScenariosUnlisted[names(allScenariosUnlisted) %in% secondGroupNames]
        diffRasComp <- mapply(FUN = .calcDiffRas, firstGroup, secondGroup, firstGroupNames, 
                              MoreArgs = list(comparison = comparison, 
                                              eachComparison = eachComparison, 
                                              sp = sp, outputFolder = outputFolder))
        scenComp <- sub(firstGroupNames, pattern = comparison[1], replacement = "")
        scenComp <- sub(scenComp, pattern = '\\.', replacement = "")
        scenComp <- sub(scenComp, pattern = '\\.', replacement = "") # The first time it doesn't work for all names?! Weird!
        names(diffRasComp) <-  paste(scenComp, eachComparison, sp, sep = "_")
# Average across the 4 scenarios
        averageAcrossScenPath <- file.path(outputFolder, paste("average", eachComparison, sp, sep = "_"))
        if (file.exists(paste0(averageAcrossScenPath, ".tif"))){
          average <- raster::raster(paste0(averageAcrossScenPath, ".tif"))
        } else {
          sbset <- raster::stack(diffRasComp)
          names(sbset) <- paste("average", eachComparison, sp, sep = "_")
          average <- calc(x = sbset, fun = mean, 
                          filename = averageAcrossScenPath, format = "GTiff")
        }
        sdAcrossScenPath <- file.path(outputFolder, paste("stdDev", eachComparison, sp, sep = "_"))
        if (file.exists(paste0(sdAcrossScenPath, ".tif"))){
          sd <- raster::raster(paste0(sdAcrossScenPath, ".tif"))
        } else {
          sbset <- raster::stack(diffRasComp)
          names(sbset) <- paste("stdDev", eachComparison, sp, sep = "_")
          sd <- calc(x = sbset, fun = sd, 
                          filename = sdAcrossScenPath, format = "GTiff")
        }
        # Saved sd but will return just the average!
      return(average)
    })
    
    browser()
    
    # 2) the difference of 1) between the scenarios fS (rasterT1) - SCFM (rasterT0) ==> For each pair of comparison, we need this...
    # 
    return(allScenarios)
  })
  
}
#   
#   HOLA_0.5_fS <- calcColonization(rasT0Path = "~/projects/NWT/outputs/14AUG20/LandR.CS_fS/run1/birdPredictionsV6a/run1_LandR.CS_fSpredictedHOLAYear2011.tif", 
#                                rasT1Path = "~/projects/NWT/outputs/14AUG20/LandR.CS_fS/run1/birdPredictionsV6a/run1_LandR.CS_fSpredictedHOLAYear2100.tif", 
#                                percentToDiscard = 0.5)
#   
#   HOLA_0.5_SCFM <- calcColonization(rasT0Path = "~/projects/NWT/outputs/14AUG20/LandR_SCFM/run1/birdPredictionsV4/run1_LandR_SCFMpredictedHOLAYear2011.tif", 
#                                   rasT1Path = "~/projects/NWT/outputs/14AUG20/LandR_SCFM/run1/birdPredictionsV4/run1_LandR_SCFMpredictedHOLAYear2100.tif", 
#                                   percentToDiscard = 0.5)
#   
#   
#   pal <- colorRampPalette(c("red","lightyellow","green"))
#   
#   
#   plot(HOLA_0.5_SCFM, col = pal(5), main = "HOLA with percentToDiscard = 0.5") #plot with defined breaks
#   tb_0.5 <- table(HOLA_0.5_SCFM[])
#   DT_0.5_SCFM <- data.table(bird = "HOLA",
#                        percentToDiscard = 0.5,
#                        # extirpation = tb_0.5["-1"], 
#                        # colonization = tb_0.5["1"], 
#                        # noChange = tb_0.5["0"],
#                        percExtirpation = tb_0.5["-1"]/sum(tb_0.5),
#                        percColonization = tb_0.5["1"]/sum(tb_0.5),
#                        percNoChange = tb_0.5["0"]/sum(tb_0.5))
#   return(list(colRasters = colRasters, colTable = colTable))
# }
# 
calcColonization <- function(rasT0Path, rasT1Path, percentToDiscard){
  rasT0 <- raster::raster(rasT0Path) # LandR_fS_v4_diff2100-2011
  rasT1 <- raster::raster(rasT1Path) # LandR_fS_v6a_diff2100-2011  ==> direct climate effect 
  # LandR_fS_v4_diff2100-2011
  # LandR_SCFM_v4_diff2100-2011
  # LandR.CS_fS_v4_diff2100-2011
  # LandR.CS_SCFM_v4_diff2100-2011
  # AVERAGE ==> 
  
  
  

  # LandR_fS_v4_2100
  # LandR_fS_v4_2011
  # LandR_fS_v6a_diff2100-2011  ==> direct climate effect 1
  # LandR_SCFM_v4_diff2100-2011
  # LandR_SCFM_v6a_diff2100-2011  ==> direct climate effect 2 
  
  # LandR.CS_fS_v4_diff2100-2011
  # LandR.CS_fS_v6a_diff2100-2011  ==> direct climate effect 3
  # LandR.CS_SCFM_v4_diff2100-2011
  # LandR.CS_SCFM_v6a_diff2100-2011  ==> direct climate effect 4
  
  
  rasT0 <- .presenceAbsenceRas(rasT0, percentToDiscard)
  rasT1 <- .presenceAbsenceRas(rasT1, percentToDiscard)
  rasCol <- rasT1 - rasT0
  return(rasCol)
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
 
.calcDiffRas <- function(firstG, secondG, groupNames, 
                         comparison, eachComparison, sp, outputFolder){
  scenComp <- sub(groupNames, pattern = comparison[1], replacement = "")
  scenComp <- sub(scenComp, pattern = '\\.', replacement = "")
  scenComp <- sub(scenComp, pattern = '\\.', replacement = "")
  rasName <- paste("diff_allRunsYears", scenComp, eachComparison, sp, sep = "_")
  rasFilename <- file.path(outputFolder, paste0(rasName, ".tif"))
  if (!file.exists(rasFilename)){
    diffRas <- firstG - secondG
    names(diffRas) <- rasName
    writeRaster(diffRas, filename = tools::file_path_sans_ext(rasFilename), 
                format = "GTiff")
  }
  return(raster::raster(rasFilename))
}