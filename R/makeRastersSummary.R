makeRastersSummary <- function(listOfRasters, 
                               studyArea,
                               field,
                               years = c(2000, 2100)){

  wholeTable <- rbindlist(lapply(X = names(listOfRasters), FUN = function(simul){
    simulLists <- listOfRasters[[simul]]
    runList <- rbindlist(lapply(X = names(simulLists), FUN = function(RUN){
      runList <- simulLists[[RUN]]
      speciesList <- rbindlist(lapply(X = names(runList), FUN = function(species){
        spStack <- runList[[species]]
        stkVals <- data.table::data.table(raster::getValues(spStack))
        dt <- rbindlist(lapply(X = names(stkVals), FUN = function(y){
          r <- stkVals[, ..y]
          YEAR <- as.numeric(usefulFuns::substrBoth(strng = y, 
                                                    howManyCharacters = 4, 
                                                    fromEnd = TRUE))
          studyAreaDT <- convertSAtoDT(studyArea = studyArea, 
                                       field = field,
                                       rasterToMatch = spStack[[1]])
          rasList <- na.omit(cbind(r, studyAreaDT))
          dt <- rbindlist(lapply(X = na.omit(unique(studyAreaDT$location)), 
                                 FUN = function(locality){
            message(crayon::blue(paste0("Calculating summary for ", 
                                        crayon::yellow(paste(simul, RUN, species, YEAR, locality,
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
      return(speciesList)
      }))
    }))
  return(wholeTable)
}