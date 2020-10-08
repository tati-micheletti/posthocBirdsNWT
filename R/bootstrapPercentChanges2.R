#' Bootstraps rasters for testing significancy on comparable rasters of different species or scenarios
#'
#' @param years numeric. Years to compare. Currently this function only compares 2 years.
#' @param dataPath character. Path to raster data.
#' @param shp character or shapefile. If you wish to calculate these metrics for separate areas.
#'            Needs to match the rasters. Default is NULL (i.e. the whole raster is only one area)
#' @param sampleSize numeric or "auto" (default). What is the sample size (i.e. number of pixels)
#'                   we want to use on the bootstrapping? If "auto" it calculates internally
#'                   Cohen's D And Hedges G Effect Size.
#' @param nBootReps numeric. Default is 100. How many iterations (random selection of `sampleSize` pixels) should be done?
#' @param species character. Default is NULL. Which species should this function be ran ?
#' @param useFuture logical. Should use future to parallelize? Requires `future` and `future_apply`` packages
#'
#' @return list of significant species or scenarios with indication of increasing or decreasing
#'
#' @author Tati Micheletti
#' @export
#' @importFrom crayon yellow green
#' @importFrom data.table data.table rbindlist
#' @importFrom effsize cohen.d
#' @importFrom future plan
#' @importFrom future.apply future_lapply
#' @importFrom pryr where
#' @importFrom raster getValues raster
#' @importFrom reproducible prepInputs Cache
#' @importFrom stats wilcox.test t.test
#'
#' @include helpersBirds.R
#' @include grepMulti.R
#' @include substrBoth.R
#' @include cbindFromList.R
#'
#' @rdname bootstrapPercentChanges2
bootstrapPercentChanges2 <- function(dataPath = NULL,
                                     listOfRasters = NULL,
                                     years = c(2011, 2100),
                                     sampleSize = "auto",
                                     nBootReps = 100,
                                     shp = NULL,
                                     species = NULL,
                                     useFuture = FALSE,
                                     simulationStamp, 
                                     overwrite = FALSE){

  if (all(is.null(dataPath), is.null(listOfRasters)))
    stop("Please supply either dataPath or listOfRasters")
  
  if (all(!is.null(dataPath), !is.null(listOfRasters)))
    stop("Please supply either dataPath or listOfRasters, not both")
  
  whichAnalysis <- ifelse(all(is.null(dataPath), 
                              !is.null(listOfRasters)),
                              "listOfRasters",
                              "dataPath")
  
    if (class(shp) == "character"){
    studyArea <- Cache(.prepStudyAreaForBirds, studyArea = shp,
                       dataPath = reproducible::checkPath(file.path(dataPath, "birdRTMEdehzhieRAS"),
                                                          create = TRUE))
  } else {
    studyArea <- shp
  }
  
  fullTable <- future_lapply(1:nBootReps, function(repetition){ # future_
    message(crayon::yellow("Starting calculateSignificantChangesInBirds for repetition ",
                           repetition, " TIME: ", Sys.time()))
    t2 <- Sys.time()
    changesTableFile <- file.path(Paths$outputPath, paste0("changesTable_rep", repetition,".qs"))
    if (any(!file.exists(changesTableFile), overwrite)){
      if (useFuture) plan("multiprocess", workers = length(species)/2)
      changesTable <- rbindlist(future_lapply(X = names(get(whichAnalysis)),
                             FUN = .calculateSignificantChangesInBirds,
                             dtList = get(whichAnalysis),
                             whichAnalysis = whichAnalysis, 
                             # This defines if the first argument is listOfRasters or dataPath
                             years = c(years[1], years[length(years)]),
                             sampleSize = sampleSize,
                             studyArea = studyArea,
                             repetition = repetition,
                             species = species,
                             simulationStamp = simulationStamp))
      message(crayon::green("FINISHED calculateSignificantChangesInBirds for repetition ",
                            repetition, " ELAPSED: ", Sys.time() - t2))
      changesTable[, repetition := unique(repetition)]
      qs::qsave(x = changesTable, file = changesTableFile)
      t2 <- Sys.time()
      plan("sequential")
    } else {
      message(crayon::green("FOUND calculateSignificantChangesInBirds Table for repetition ",
                            repetition, " ELAPSED: ", Sys.time() - t2))
      changesTable <- qs::qread(file = changesTableFile)
    }
    percentChange <- .calculatePercentageChanges(changesTable = changesTable, 
                                                 column = "result")
    percentChange[, repetition := unique(repetition)]
    return(list(changesTable = changesTable, percentChange = percentChange))
  })
  changesTableList <- lapply(fullTable, '[[',"changesTable")
  constantSpecies <- .whichSpeciesChange(changesTable = changesTableList)
  percChange <- data.table::rbindlist(lapply(fullTable, '[[',"percentChange"))
  if (!is.null(studyArea)){
    dt <- data.table::rbindlist(lapply(unique(percChange[["location"]]), function(locality){
      dt <- data.table::rbindlist(lapply(unique(percChange[["scenario"]]), function(Scenario){
      percChangeLoc <- percChange[location == locality & scenario == Scenario,]
      dt <- .calcICPercentChange(percChange = percChangeLoc)
      dt[["location"]] <- locality
      dt[["scenario"]] <- Scenario
      return(dt)
    }))
  }))
  } else {
    dt <- .calcICPercentChange(percChange = percChange)
  }
  return(list(constantSpecies = constantSpecies, tableIC = dt))
}
