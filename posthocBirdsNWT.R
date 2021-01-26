defineModule(sim, list(
  name = "posthocBirdsNWT",
  description = paste0("Posthoc module specifically for the manuscript: ",
                       "The future of the Northern Boreal Forest bird ",
                       "communities: accounting for the lag in vegetation ",
                       "response to climate change.", 
                       "What this module does:",
                       "1) rasters of difference between last and fist year (relative or absolute)",
                       " - delta rasters ",
                          "-- makeDeltaRasters(), ",
                       "2) calculates significant changes of list of rasters per year (i.e. ",
                       "species) using bootstrapping through pixels, and a summary per polygon ",
                          "-- bootstrapPercentChanges(), ",
                      "3) Make plot and summary of average values and deviations through time", 
                      "for each polygon in a shapefile ",
                          "-- makeDiffAnalysis2()",
                          "-- makeRastersSummary()",
                       # "4) Make a GIF with predictions rasters"), # Not currently available
                       "5) Calculate the average and error across replicates for polygons ",
                          "-- makeAveragePlotTime()",
                       "6) Create a cummulative effects raster of climate change ",
                      "(veg+fire+directEffect), summarizing across runs",
                          "-- createCumEffRasters()"),
  keywords = c("rasters", "posthoc analysis", "plots", "predictions"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", 
                     role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.1", posthocBirdsNWT = "0.0.0.9000"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "posthocBirdsNWT.Rmd"),
  reqdPkgs = list("raster", 
                  "reproducible",
                  "data.table", 
                  "future", 
                  "future.apply", 
                  "tictoc",
                  "ggplot2",
                  "googledrive",
                  "tati-micheletti/usefulFuns@fileMystery"
                  ),
  parameters = rbind(
    defineParameter("simulationStamp", "character", "TEST", NA, NA,
                    paste0("Stamp on the simulation to ID the run.")),                 
    defineParameter("species", "character", "CAWA", NA, NA, 
                    paste0("Which species to use? If NULL, defaults to CAWA.")),
    defineParameter("years", "numeric", c(seq(2011, 2100, by = 10), 2100), NA, NA, 
                    paste0("Years to use in the functions. For the delta rasters, ",
                           "it will use the first and last years supplied here")),
    defineParameter("relativeDelta", "logical", TRUE, NA, NA, 
                    "Should the delta rasters be relative to the first one?"),
    defineParameter("patternsToRetrieveRasters", "character", c("predicted", ".tif"), NA, NA, 
                    paste0("Which patterns should be used to find the rasters in the dataFolder?",
                           "As this is a specific bird's posthoc module, the default is ",
                           "'c('predicted', '.tif')'")),
    defineParameter("sampleSize", "character|numeric", "auto", NA, NA, 
                    paste0("How many pixels should be used for the bootstrapping as sample size? ",
                           "If 'auto', it uses cohen.d() to calculate an ideal sample size ",
                           "that reduces the effect of big sizes on significancy")),
    defineParameter("nBootReps", "numeric", 2, NA, NA, 
                    paste0("How many repetitions for the bootstrapping should be run ? ",
                           "The higher the repetition number, the longer it will take to run")),
    defineParameter("makeRasterChangeGIF", "logical", FALSE, NA, NA, 
                    paste0("Should the gif be made ? It takes a bit of time if TRUE",
                           " AS OF 30th OCT 19 IT IS STILL NOT IMPLEMENTED")), 
    defineParameter("overwriteDelta", "logical", FALSE, NA, NA, 
                    paste0("Should the deltas maps be overwritten?")),
    defineParameter("overwriteBootstrap", "logical", FALSE, NA, NA, 
                    paste0("Should the bootstrapping be overwritten?")),
    defineParameter("uploadPlots", "logical", FALSE, NA, NA, 
                    paste0("Should the plots be uploaded to ggdrive?")),
    defineParameter("typeOfAnalysis", "character", "generic", NA, NA, 
                    paste0("Type of analysis for naming purposes")),
    defineParameter("eventsToSchedule", "character", c("makeDeltaRasters","makeSummary",
                                                       "averageThroughTimeComparison"), 
                    NA, NA, 
                    paste0("Which events should happen?",
                           " Defaults to basic ones")),
     defineParameter("plotCI", "logical", FALSE, NA, NA, 
                    paste0("Should it plot the CI",
                           " on the average through time plots? Remember there is a lot ",
                           "of spatial variation")),
    defineParameter("useFuture", "logical", FALSE, NA, NA, 
                    paste0("Should future (parallel processing)",
                           "be used for the calculations?")),
    defineParameter("shpFieldToUse", "character", "REGION_NAM", NA, NA, 
                    paste0("Which field of the shapefile indicates spatial",
                           "units to be used for plotting and summarizing?")),
    defineParameter("percentToDiscard", "numeric", 0.3, NA, NA, 
                    paste0("What is the proportion of bird abundance values that should",
                           " be discarded to consider presence and absence? Defaults to 30% - 0.3",
                           ""))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = "dataFolder", objectClass = "list", 
                 desc = paste0("This is a named list of the folders where the results should be.",
                               " (i.e. `folders[['LandR.CS_fS']] = file.path(getwd(),",
                               "'outputs/DATE/LandR.CS_fS/birdPrediction')`)",
                               " Pass as a vector of locations for running the functions in ",
                               "more than one location, as it lapplies through it internally. ",
                               "The names should be the type of simulation ran. If this ",
                               "is not supplied, you have to supply the list of rasters, ",
                               "organized in the same way"), 
                 sourceURL = NA),
    expectsInput(objectName = "listOfRasters", objectClass = "list", 
                 desc = paste0("This is a named 3 level list of the ",
                               # originalDateAnalysis --> comparison --> runs --> birdModels # <== THIS CHANGED! CONFIRM THE ORDER!
                               "1) location or simulation type, 2) objects to lapply ",
                               "through (i.e. can be a list of bird species, which one ",
                               "containing the 3) time series of rasters to analyze). ",
                               "This list can also be of just one location/simulation, ",
                               "and/or one species (i.e. caribou)",
                               " or another 'object' (i.e. temperature)"),
                 sourceURL = NA),
    expectsInput(objectName = "googleFolders", objectClass = "list", 
                 desc = paste0("This is a named folder to upload the results. ",
                               "There are no defaults, needs to be provided"),
                 sourceURL = NA),
    expectsInput(objectName = "comparisons", objectClass = "list", 
                 desc = paste0("This is a named list of the comparisons to make ",
                               "(i.e. LandR.CS_fS and LandR_SCFM)", 
                               "IMPORTANT: In the comparisons, always the climate sensitives",
                               " need to come first! This is expected by the internal functions"),
                 sourceURL = NA),
    expectsInput(objectName = "predictedRastersFolder", objectClass = "character", 
                 desc = paste0("This is the path to the folder that holds the predicted rasters ",
                               "(i.e. ~/projects/NWT/outputs/DDMMMYY)"),
                 sourceURL = NA),
    expectsInput(objectName = "studyAreaPosthoc", objectClass = "SpatialPolygonsDataFrame", 
                 desc = paste0("Shapefile to be used to summarize the results for tables ",
                               "and plots"),
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "deltaRasters", objectClass = "list", 
                  desc = "List of delta rasters"),
    createsOutput(objectName = "significantChanges", objectClass = "list", 
                  desc = paste0("List of several results showing which one ",
                                "(i.e. species, if a list) significantly increased,",
                                "decreased or didn't change")),
    createsOutput(objectName = "pixelsSummaries", objectClass = "list", 
                  desc = "Summary of pixelValues (bird density!) mean, max, min, median per year"), 
    createsOutput(objectName = "gifFigure", objectClass = "list", 
                  desc = "List of gifFigures delta rasters"), 
    createsOutput(objectName = "differenceRasters", objectClass = "list", 
                  desc = paste0("List of rasters differences between the scenarios ",
                  "(i.e. LandR.CS_fS and LandR_SCFM) BY run (it uses the 'run' to compare), ",
                  "and then it takes the average of the differences across runs and standard ",
                  "deviation in the form of maps.  ")),
    createsOutput(objectName = "cummEffRas", objectClass = "list", 
                  desc = paste0("List of averaged and deviation of cumulative effects raster for ",
                                "each bird")),
    createsOutput(objectName = "colonizationRasters", objectClass = "list", 
                  desc = paste0("List of colonization/extirpation rasters (from 2011 to 2100) for ",
                                "each bird and scenario. Runs are all considered? Averaged?"))
  )
))

doEvent.posthocBirdsNWT = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # Create cohen.D output file
      cat(paste("species", "scenario", "location", "sampleSize", "cohenEstimate", "effectSize", 
                collapse = ";"), 
          file = file.path(Paths$outputPath, paste0("cohenD_", 
                                                    P(sim)$simulationStamp, 
                                                    ".txt")), 
          sep = "\n")
      
      # schedule future event(s)
      if ("makeDeltaRasters" %in% P(sim)$eventsToSchedule){
      sim <- scheduleEvent(sim, start(sim), "posthocBirdsNWT", "makeDeltaRasters")
      }
      if ("calculatesSignificantChanges" %in% P(sim)$eventsToSchedule){
        sim <- scheduleEvent(sim, start(sim), "posthocBirdsNWT", "calculatesSignificantChanges")
      }
      if ("makeSummary" %in% P(sim)$eventsToSchedule){
        sim <- scheduleEvent(sim, start(sim), "posthocBirdsNWT", "makeSummary")
      }
      # sim <- scheduleEvent(sim, start(sim), "posthocBirdsNWT", "makeGIF") # Currently not implemented
      if ("generateRSFbinned" %in% P(sim)$eventsToSchedule){
        sim <- scheduleEvent(sim, start(sim), "posthocBirdsNWT", "generateRSFbinned")
      }
      if ("averageThroughTimeComparison" %in% P(sim)$eventsToSchedule){
      sim <- scheduleEvent(sim, start(sim), "posthocBirdsNWT", "averageThroughTimeComparison")
      }
    },
    makeDeltaRasters = {
      message("Running makeDeltaRasters")
      sim$deltaRasters <- makeDeltaRasters(listOfRasters = sim$listOfRasters,
                                           relativeDelta = P(sim)$relativeDelta,
                                           years = P(sim)$years,
                                           species = P(sim)$species,
                                           outputFolder = Paths$outputPath,
                                           upload = P(sim)$uploadPlots,
                                           folderID = sim$googleFolders,
                                           overwrite = P(sim)$overwriteDelta,
                                           useFuture = P(sim)$useFuture)
      
      # This is the delta between the first (i.e. 2011) and last (i.e. 2100) layers, 
      # for each species, scenario (CC and noCC),
      # for each individual run! 
      # # This will take 10 seconds per map = 64sp*2birdModels*4scenarios*10runs = @15hs
            message("Running calcColonizationList")
      sim$colonizationRasters <- calcColonizationList(listOfRasters = sim$listOfRasters, 
                                               years = P(sim)$years,
                                               species = P(sim)$species,
                                               outputFolder = Paths$outputPath,
                                               comparisons = sim$comparisons,
                                               overwrite = P(sim)$overwriteColras,
                                               useFuture = P(sim)$useFuture,
                                               percentToDiscard = P(sim)$percentToDiscard)
      # This function will calculate which species colonized and which
      # species disappeared from a pixel with time, considering the factorial example.

    },
    calculatesSignificantChanges = {
      message("Running bootstrapPercentChanges")
      sim$significantChanges <- bootstrapPercentChanges2(listOfRasters = sim$listOfRasters,
                                                        years = P(sim)$years,
                                                        sampleSize = P(sim)$sampleSize, 
                                                        nBootReps = P(sim)$nBootReps,
                                                        shp = sim$studyAreaPosthoc,
                                                        species = P(sim)$species,
                                                        useFuture = P(sim)$useFuture,
                                                        simulationStamp = P(sim)$simulationStamp,
                                                        overwrite = P(sim)$overwriteBootstrap)
      
      # This is the bootstrapped wilcox test on pixels between 2011 and 2100, per location 
      # (study area shapefile), per run, per scenario per bird, internally. It returns 
      # 1) CI on % of species that the changed and the direction of the change per location, 
      # per scenario. 
      # 2) Which birds species consistently decreased, increased or didn't change in density 
      # per location, per scenario. 
    },
    makeSummary = {
      # sim$differenceRasters <- lapply(seq_along(sim$comparisons), function(index){
      #   message(paste0("Running makeDiffAnalysis for ", sim$comparisons[index]))
      #   diffRasters <- makeDiffAnalysis2(predictedRastersFolder = sim$predictedRastersFolder,
      #                           resultsFolder = Paths$outputPath,
      #                           allRasters = sim$birdRasters, # If NULL, the factorial rasters
      #                           # based on the arguments below are loaded
      #                           Run = P(sim)$runs,
      #                           Species = P(sim)$species,
      #                           Year = P(sim)$years,
      #                           typeOfSpecies = "bird",
      #                           SpeciesScenario = P(sim)$birdModels,
      #                           comparisons = sim$comparisons[index],
      #                           writeRas = TRUE,
      #                           useFuture = P(sim)$useFuture
      #                           # , overwrite = TRUE
      #   )
      # })
      # names(sim$differenceRasters) <- names(sim$comparisons)

      # This function calculates the differences between the scenarios 
      # (i.e. LandR.CS_fS and LandR_SCFM) BY run (it uses the 'run' to 
      # compare), and then it takes the average of the differences 
      # across runs and standard deviation in the form of maps.  
      message("Running makeRastersSummary")
      sim$pixelsSummaries <- makeRastersSummary(listOfRasters = sim$listOfRasters,
                                                studyArea = sim$studyAreaPosthoc,
                                                species = P(sim)$species,
                                                field = P(sim)$shpFieldToUse,
                                                useFuture = P(sim)$useFuture,
                                                years = P(sim)$years)

      # This function returns individual pixel values  
      # for each scenario, run, year, and species BY location.
      # list of a DT for each species, with the following columns:
      # pixelID, Year, Run, location, 
      # LandR.CS_fS_V4, LandR.CS_SCFM_V4, LandR_fS_V4, LandR_SCFM_V4,
      # LandR.CS_fS_V6a, LandR.CS_SCFM_V6a, LandR_fS_V6a, LandR_SCFM_V6a
    },
    makeGIF = {
      #TODO sim$gif Figure NOT YET IMPLEMENTED. 
      # DON'T HAVE A GENERIC FUNCTION TO GENERATE THE GIF FROM R
    },

    averageThroughTimeComparison = {
      message("Running plotAbundanceThroughTime")
      sim$averageTimePlot <- plotAbundanceThroughTime(pixelsSummaries = sim$pixelsSummaries,
                                                 useFuture = P(sim)$useFuture,
                                                 overwrite = FALSE,
                                                 comparisons = c(sim$comparisons, 
                                                                 list(netEffect = c("LandR.CS_fS_V6a", 
                                                                                    "LandR_SCFM_V4"))), 
                                                 # SHOULD TEST IF THIS WORKS WITH COLONIZATION TOO! 
                                                 # CURRENTLY DOING A SPECIFIC ONE FOR NET EFFECT! 
                                                 locations = 1:length(unique(sim$studyAreaPosthoc[[P(sim)$shpFieldToUse]])),
                                                 years = P(sim)$years,
                                                 pathOutputs = Paths$outputPath)
      
      # This plot shows the averaged effect (CC-noCC) mean over area (studyAreaPosthoc) across all runs.
      # It's important to note that there is a deviance (sd) already calculated, but the CI shown 
      # here is the deviance from the averaged mean (i.e. inside each polygon, I have lots of 
      # pixels and each pixel is a mean of the difference between CC-noCC, across all runs). 
      # It is a 3 colour plot, where: 
        # 1. the mean is a line, 
        # 2. the CI of the averaged mean is one color
        # 3. the averaged SD is a lighter colour
# X. the min and max of that, is yet another one even lighter --> Way too much variation. Scratched!
    # sim$cummEffRas <- createCumEffRasters(species = P(sim)$species,
    #                       rasFolder = Paths$outputPath) 
    #                       # ==> not done as needs to do it per year and per type of simulation!
    
    # These rasters per species show the (i) averaged effect of climate change across runs, and 
    # (ii) its deviation.
    
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if(!suppliedElsewhere("dataFolder", sim)){
    if(!suppliedElsewhere("listOfRasters", sim)){
      stop("You must supply either a listOfRasters or a dataFolder")
    }
  }

  if(!suppliedElsewhere("listOfRasters", sim)){
    if(!suppliedElsewhere("dataFolder", sim)){
      stop("You must supply either a listOfRasters or a dataFolder")
    }
    sim$listOfRasters <- retrieveRasters(dataFolder = sim$dataFolder,
                                         years = P(sim)$years, 
                                         patternsToRetrieveRasters = P(sim)$patternsToRetrieveRasters, 
                                         species = P(sim)$species) 
  }
  if (isTRUE(P(sim)$uploadPlots)){
    if(!suppliedElsewhere("googleFolders", sim)){
      stop(paste0("If you set upload to TRUE, you need to provide a named ",
                  "folder (i.e. scenarios) with the google ids for uploading"))
    }
  }
  
  return(invisible(sim))
}
