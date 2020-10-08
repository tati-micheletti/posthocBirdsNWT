makeRastersSummary <- function(listOfRasters, 
                               studyArea,
                               species,
                               field,
                               useFuture,
                               years = c(2000, 2100)){
  if (!field %in% names(studyArea))
    stop("The parameters shpFieldToUse is not a variable in studyArea")
  if (useFuture) plan("multiprocess", workers = length(species)/2)
  wholeTable <- rbindlist(future_lapply(X = names(listOfRasters), FUN = function(species){
    speciesLists <- listOfRasters[[species]]
    simulList <- rbindlist(lapply(X = names(speciesLists), FUN = function(simul){
      simulList <- speciesLists[[simul]]
      bmodList <- rbindlist(lapply(X = names(simulList), FUN = function(bmod){
        bmodList <- simulList[[bmod]]
        runsList <- rbindlist(lapply(X = names(bmodList), FUN = function(runs){
          runsList <- bmodList[[runs]]
        stkVals <- data.table::data.table(raster::getValues(runsList))
        dt <- rbindlist(lapply(X = names(stkVals), FUN = function(y){
          r <- stkVals[, ..y]
          YEAR <- as.numeric(usefulFuns::substrBoth(strng = y, 
                                                    howManyCharacters = 4, 
                                                    fromEnd = TRUE))
          studyAreaDT <- convertSAtoDT(studyArea = studyArea, 
                                       field = field,
                                       rasterToMatch = runsList[[1]])
          rasList <- na.omit(cbind(r, studyAreaDT))
          dt <- rbindlist(lapply(X = na.omit(unique(studyAreaDT$location)), 
                                 FUN = function(locality){
            message(crayon::blue(paste0("Calculating summary for ", 
                                        crayon::yellow(paste(species, simul, bmod, runs, 
                                                             YEAR, locality,
                                                             collapse = " ")))))
            locName <- attr(studyAreaDT, "conversionTable")
            locName <- locName[regionID == locality, regionName]
            dt <- data.table(scenario = simul,
                             species = species,
                             location = locality,
                             locationName = locName,
                             run = RUN,
                             year = YEAR,
                             mean = rasList[location == locality, mean(get(y), na.rm = TRUE)],
                             min = rasList[location == locality, min(get(y), na.rm = TRUE)],
                             max = rasList[location == locality, max(get(y), na.rm = TRUE)],
                             median = rasList[location == locality, median(get(y), na.rm = TRUE)],
                             sd = rasList[location == locality, sd(get(y), na.rm = TRUE)])
            return(dt)
          }))
        return(dt)
      }))
        return(dt)
        }))
        return(runsList)
      }))
      return(bmodList)
    }))
    return(simulList)
  }))
  plan("sequential")
  savePath <- reproducible::checkPath(path = file.path(Paths$outputPath, "summaryRasters"), 
                                      create = TRUE)
  qs::qsave(wholeTable, file = file.path(savePath, "rastersSummaryTable.qs"))
  return(wholeTable)
}