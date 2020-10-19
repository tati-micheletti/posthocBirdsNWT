makeRastersSummary <- function(listOfRasters, 
                               studyArea,
                               species,
                               field,
                               useFuture,
                               years = c(2000, 2100)){
  if (!field %in% names(studyArea))
    stop("The parameters shpFieldToUse is not a variable in studyArea")
  if (useFuture) plan("multiprocess", workers = length(species)/4)
  wholeTable <- future_lapply(X = names(listOfRasters), FUN = function(species){ # future_lapply <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tic(paste0("Elapsed time for ", sp))
    savePath <- reproducible::checkPath(path = file.path(Paths$outputPath, "summaryRasters"), 
                                        create = TRUE)
    speciesTable <- file.path(savePath, paste0(species,"_rastersSummaryTable.qs"))
    if (file.exists(speciesTable)){
        message(paste0("Full table exists for ", species, ". Returning path..."))
    } else {
      speciesLists <- listOfRasters[[species]]
      simulList <- lapply(X = names(speciesLists), FUN = function(simul){
        tic(paste0("Elapsed time for ", simul, " for ", sp))
        simulList <- speciesLists[[simul]]
        bmodList <- lapply(X = names(simulList), FUN = function(bmod){
          bmodList <- simulList[[bmod]]
          firstIteration <- ifelse(all(names(speciesLists)[1] == simul,
                                       names(simulList)[1] == bmod), TRUE, FALSE)
          if (firstIteration){
            runsList <- rbindlist(lapply(X = names(bmodList), FUN = function(runs){
              runsList <- bmodList[[runs]]
              stkVals <- data.table::data.table(raster::getValues(runsList))
              dt <- rbindlist(lapply(X = names(stkVals), FUN = function(y){
                YEAR <- as.numeric(usefulFuns::substrBoth(strng = y, 
                                                          howManyCharacters = 4, 
                                                          fromEnd = TRUE))
                message(crayon::blue(paste0("Making table for ",
                                            crayon::yellow(paste(species, paste0(simul, "_", bmod), 
                                                                 runs,
                                                                 YEAR,
                                                                 collapse = " ")))))
                r <- stkVals[, ..y]
                studyAreaDT <- convertSAtoDT(studyArea = studyArea, 
                                             field = field,
                                             rasterToMatch = runsList[[1]])
                LENGTH <- as.numeric(NROW(studyAreaDT))
                # studyAreaDT[, pixelID := as.numeric(1:LENGTH)]
                studyAreaDT$pixelID <- 1:LENGTH
                rasList <- na.omit(cbind(r, studyAreaDT), cols = names(r))
                setnames(x = rasList, 
                         old = names(rasList)[names(rasList) == names(r)], 
                         new = paste0(simul,"_",bmod))
                # Add run and year
                rasList[, Run := runs]
                rasList[, Year := YEAR]
                setattr(rasList, "birdSpecies", species)
                return(rasList)
              }))
              return(dt)
            }))
          } else {
            runsList <- rbindlist(lapply(X = names(bmodList), FUN = function(runs){
              runsList <- bmodList[[runs]]
              stkVals <- data.table::data.table(raster::getValues(runsList))
              dt <- rbindlist(lapply(X = names(stkVals), FUN = function(y){
                YEAR <- as.numeric(usefulFuns::substrBoth(strng = y, 
                                                          howManyCharacters = 4, 
                                                          fromEnd = TRUE))
                message(crayon::blue(paste0("Making following tables for ",
                                            crayon::yellow(paste(species, paste0(simul, "_", bmod),
                                                                 runs,
                                                                 YEAR,
                                                                 collapse = " ")))))
                r <- stkVals[, ..y]
                setnames(x = r,
                         old = names(r),
                         new = paste0(simul,"_",bmod))
                return(na.omit(r))
              }))
              return(dt)
            }))
          }
          return(runsList)
        })
        bmodList <- setDT(unlist(bmodList, recursive = FALSE), check.names = FALSE)
        toc()
        return(bmodList)
      })
      finalSpeciesTable <- setDT(unlist(simulList, recursive = FALSE), check.names = FALSE)
      qs::qsave(finalSpeciesTable, file = speciesTable)
      rm(finalSpeciesTable)
      gc()
    }
    toc()
    return(speciesTable)
  })
  plan("sequential")
  names(wholeTable) <- names(listOfRasters)
  return(wholeTable)
}

# dt <- rbindlist(lapply(X = na.omit(unique(studyAreaDT$location)), 
#                        FUN = function(locality){
#                          message(crayon::blue(paste0("Calculating summary for ", 
#                                                      crayon::yellow(paste(species, simul, bmod, runs, 
#                                                                           YEAR, "location", locality,
#                                                                           collapse = " ")))))
#                          locName <- attr(studyAreaDT, "conversionTable")
#                          locName <- locName[regionID == locality, regionName]
#                          SIMUL <- unlist(strsplit(x = simul, split = "_", fixed = TRUE))
#                          dt <- data.table(vegetation = SIMUL[1],
#                                           fire = SIMUL[2],
#                                           birdModel = bmod,
#                                           species = species,
#                                           location = locality,
#                                           locationName = locName,
#                                           run = runs,
#                                           year = YEAR,
#                                           mean = rasList[location == locality, mean(get(y), na.rm = TRUE)],
#                                           min = rasList[location == locality, min(get(y), na.rm = TRUE)],
#                                           max = rasList[location == locality, max(get(y), na.rm = TRUE)],
#                                           median = rasList[location == locality, median(get(y), na.rm = TRUE)],
#                                           sd = rasList[location == locality, sd(get(y), na.rm = TRUE)])
#                          
#                          return(dt)
#                        }))