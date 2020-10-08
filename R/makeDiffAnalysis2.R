# 5. Difference between "non-climate sensitive" ones - "climate sensitive" ones 
# (i.e. LandR.CS_fS_V6_climateStatic - LandR.CS_fS_V6)
# 6. Then I average these differences, and these are the average climate/vegetation/fire 
# effect on birds through time.
# 7. If these differences (maps) are not spatially varying, I can make a plot of the 
# average of the pixels, with SD. (edited) 

# lapply through birds, then years (outter loops) 
# for each ras do the difference operation accross the same runs (so it can take as many as I have)


# FOR FACTORIAL: These rasters are 'repetitions' both on the runs and on the climate 
# side (i.e. each one of the scenarios, varying only climate).

# FOR NON-FACTORIAL:  These rasters are 'repetitions' on the runs
# When I add the coefficient of variation, this will have to be done at the 'run' level. 
# I will need to compare the different rasters of the same runs.



makeDiffAnalysis2 <- function(predictedRastersFolder = file.path(getwd(), "outputs/06DEC19"),
                              resultsFolder = tempdir(),
                              allRasters = NULL, # If you want a factorial combination, just leave this NULL!
                             Species = c("CAWA", "OSFL", "RUBL"),
                             Year = c(2011, 2041, 2071, 2100),
                             Scenario = c("LandR_fS", "LandR_SCFM", "LandR.CS_fS", "LandR.CS_SCFM"),
                             SpeciesScenario = NULL, # NULL for caribou, c("V4", "V6") for birds 
                             Run = c("run1", "run2"),#, "run3"
                             comparisons, writeRas = FALSE, 
                             returnAllRasters = FALSE,
                             typeOfSpecies = "bird", # or caribou
                             overwrite = FALSE,
                             useFuture = FALSE, ...){
  
  outputFolder <- checkPath(file.path(resultsFolder, "effectsRasters"), create = TRUE)
  if (!is(comparisons, "list")|is.null(names(comparisons)))
    stop("Comparisons need to be a named list of what you are making the differences")
  if (length(comparisons[[names(comparisons)]])>2)
    stop("Comparisons can only be made for 2 groups for now")
  tic("All Raster loaded! Difference analysis finished: ")
  # allRasters: Flat unnamed list of all rasters that I want to compare
if (is.null(allRasters)){
  if (useFuture) plan("multiprocess", workers = length(Species)/2)
  allRasters <- unlist(future_lapply(Species, function(species){ # future_lapply
    birdRasters <- lapply(Year, function(year){
      birdYearRasters <- lapply(Scenario, function(scenario){
        if (is.null(SpeciesScenario)) SpeciesScenario <- "NOT_AVAILABLE"
        birdYearScenario <- lapply(SpeciesScenario, function(speciesScenario){
          birdYearScenarioSpeciesScen <- lapply(Run, function(run){
            if (any(SpeciesScenario == "NOT_AVAILABLE"))
              speciesScenario <- ""
            ras <- file.path(predictedRastersFolder, scenario, 
                             run, paste0(typeOfSpecies, "Predictions", 
                                         speciesScenario), 
                             ifelse(typeOfSpecies == "bird",
                                    paste0(run, "_", scenario, "predicted", 
                                           species, "Year", year, ".tif"),
                                    paste0("relativeSelectionTaigaPlains_Year",
                                           year, ".tif")))
            if (file.exists(ras)){
              message(paste("Raster for", species, year, scenario, 
                            speciesScenario, run, "exists. Returning...", 
                            collapse = " "))
              ras <- raster(ras)
            } else {
              message(paste0("Apparently file doesn't exist. Are you sure",
                             " the path is correct? ", ras))
              browser()
            }
            names(ras) <- paste0(species, year, scenario, speciesScenario, run)
            return(ras)        
          })
        })
      })
    })
  }))
  plan("sequential")
}
  message(crayon::green("All Raster loaded! Starting difference analysis..."))
  # 3 birds x 4 years x 3 runs x 8 scenarios = 288 rasters
   
  # Only comparing the two extremes: LandR.CS + fS + V6a vs LandR + SCFM + V4
  # --> For paper: 64 birds x 6 years x 10 runs x 2 scenarios = 7,680 rasters
  
  # The full on:
  # --> For paper: 64 birds x 6 years x 10 runs x 8 scenarios = 30,720 rasters
  
  # 1 caribou x 3 years x 3 runs x 4 scenarios = 36 rasters
  # --> For paper: 1 caribou x 6 years x 10 runs x 2 scenarios = 120 rasters
  # Name the raster list
  allRastersNames <- unlist(lapply(allRasters, FUN = names))
  names(allRasters) <- allRastersNames
  
  # lapply through birds, and then years for the difference maps
    if (useFuture) plan("multiprocess", workers = length(Species)/2)
  allSpeciesDiffMaps <- future_lapply(X = Species, FUN = function(species){
    message(crayon::yellow(paste0("Producing maps for ", species)))
    tic(paste0("All maps for ", species, " produced:"))
    oneSpecies <- allRasters[names(allRasters) %in% grepMulti(x = names(allRasters), 
                                                              patterns = species)]
    oneYear <- lapply(Year, FUN = function(year){
      tic(paste0("Producing maps for ", species, " for ", year))
      averageName <- paste0(species, year, names(comparisons), "_mean")
      sdName <- paste0(species, year, names(comparisons), "_sd")
      if (all(file.exists(file.path(outputFolder, 
                                    paste0(averageName, ".tif"))), 
              file.exists(file.path(outputFolder,
                                    paste0(sdName, ".tif"))))){
        message(crayon::yellow(paste0("Difference maps for ", species, 
                                      " for year ", year,
                                      " for ", names(comparisons),  
                                      " comparison exist. Returning.")))
        return(list(averageDifference = file.path(outputFolder, 
                                                  paste0(averageName, ".tif")), 
                    sdDifference = file.path(outputFolder,
                                             paste0(sdName, ".tif"))))
      } else {
        oneYear <- oneSpecies[names(oneSpecies) %in% 
                                grepMulti(x = names(oneSpecies), 
                                          patterns = year)]
        oneRun <- unlist(lapply(Run, FUN = function(run){# future_lapply
          oneRun <- oneYear[names(oneYear) %in% 
                              grepMulti(x = names(oneYear), patterns = run)]
          group1 <- oneRun[names(oneRun) %in% 
                             grepMulti(x = names(oneRun),
                                       patterns = comparisons[[names(comparisons)]][1])]
          group2 <- oneRun[names(oneRun) %in% 
                             grepMulti(x = names(oneRun),
                                       patterns = comparisons[[names(comparisons)]][2])]
          if (length(group1) != length(group2))
            stop("The lengths of the groups of rasters don't match. 
               Please debug and make sure the sequence of rasters is correct")
          diffRas <- lapply(X = seq_along(group1), FUN = function(index){
            tic("Calculating difference rasters")
            diffRas <- group2[[index]] - group1[[index]]
            diffName <- gsub(x = names(group2[[index]]), 
                             pattern = comparisons[[names(comparisons)]][1], replacement = "")
            diffName <- gsub(x = diffName, 
                             pattern = comparisons[[names(comparisons)]][2], replacement = "")
            names(diffRas) <- paste0(diffName, "_", names(comparisons))
            toc()
            return(diffRas)
          })
        }))
        
        browser() # Plot all the repetitions, and fit the model to the "raw" data, not the mean
        # Of the area burned across all reps. Same for all the plots!
        # Jitter/ density scatterplot, but not average!
        
        oneRunNm <- unlist(lapply(oneRun, FUN = names))
        names(oneRun) <- oneRunNm
        message("Calculating average of raster differences for ", 
                paste(species, year, names(comparisons), collapse = " "))
        tic("Average and sd calculations")
        averageReps <- calc(stack(oneRun), fun = mean, na.rm = TRUE)
        sdReps <- calc(stack(oneRun), fun = sd, na.rm = TRUE)
        names(averageReps) <- averageName
        names(sdReps) <- sdName
        toc()
          message("Writting rasters for ", paste(species, year, 
                                                 names(comparisons), 
                                                 collapse = " "))
          writeRaster(x = averageReps, filename = file.path(outputFolder, 
                                                            paste0(averageName, 
                                                                   ".tif")), 
                      format = "GTiff", overwrite = overwrite)
          writeRaster(x = sdReps, filename = file.path(outputFolder, 
                                                       paste0(sdName, 
                                                              ".tif")), 
                      format = "GTiff", overwrite = overwrite)
        toc()
        return(list(averageDifference = averageName, 
                    sdDifference = sdName))
      }
    })
    names(oneYear) <- paste0("Year", Year)
    toc()
    return(oneYear)
  })
  plan("sequential")
  names(allSpeciesDiffMaps) <- Species
  toc()
  
  if (!returnAllRasters){
    return(allSpeciesDiffMaps)
  } else {
    return(list(allRasters = allRasters, 
                diffRas = allSpeciesDiffMaps))
  }
}