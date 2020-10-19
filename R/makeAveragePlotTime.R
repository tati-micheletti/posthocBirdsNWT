makeAveragePlotTime <- function(dataFolder, # Where the mean rasters are (effectsRasters)!
                                Species, 
                                scenarios,
                                years,
                                field,
                                useFuture,
                                rasterToMatch,
                                useGeneralNWTclass = FALSE,
                                shp = NULL, 
                                makeIndividualEffects = TRUE,
                                makeIndividualEffectsByPolygon = TRUE,
                                makeCumEffects = TRUE,
                                makeCumEffectsByPolygon = TRUE,
                                overwrite = FALSE){
  
  averageTimePlotTablePath <- file.path(dataFolder, "birds_AverageTimePlotTable.rds")
  if (all(file.exists(averageTimePlotTablePath), !isTRUE(overwrite))){
    plotMaps <- readRDS(averageTimePlotTablePath)
  } else {
    plotMaps <- rbindlist(lapply(scenarios, FUN = function(scen){ # CAN ONLY USE 22 SPECIES AT A TIME, MAYBE LESS!
      # if (useFuture) plan("multiprocess", workers = length(Species)/3) # <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ future_lapply
      plotMaps <- rbindlist(lapply(Species, FUN = function(sp){ # <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ future_lapply
        birdFiles <- grepMulti(x = list.files(dataFolder, full.names = T, recursive = TRUE), 
                               patterns = c(sp, scen, "_mean.tif"))
        if (length(birdFiles) == 0) return(NULL)
        message("Loading ", sp, " averaged difference rasters..")
        birdRasList <- stack(lapply(birdFiles, raster))
        names(birdRasList) <- paste0(sp, years)
        birdFiles <- grepMulti(x = list.files(dataFolder, full.names = T, recursive = TRUE), 
                               patterns = c(sp, scen, "_sd.tif"))
        if (length(birdFiles) == 0) return(NULL)
        message("Loading bird standard deviation difference rasters..")
        birdRasListSD <- stack(lapply(birdFiles, raster))

        birdRasList <- unstack(birdRasList)
        names(birdRasList) <- lapply(birdRasList, names)
        birdRasListSD <- unstack(birdRasListSD)
        names(birdRasListSD) <- lapply(birdRasList, names)
        
        if (!is.null(shp)){
          studyAreaDT <- convertSAtoDT(studyArea = shp, 
                          field = field, 
                          rasterToMatch = birdRasList[[1]])
        }
        
        dt <- rbindlist(lapply(seq_along(birdRasList), function(index){
          ras <- birdRasList[[index]]
          r <- raster::getValues(ras)
          rasSD <- birdRasListSD[[index]]
          rSD <- raster::getValues(rasSD)
          
          YEAR <- usefulFuns::substrBoth(strng = names(ras), 
                                         howManyCharacters = 4, 
                                         fromEnd = TRUE)
          if (is.null(shp)){
            studyAreaDT <- r
            studyAreaDT[!is.na(studyAreaDT)] <- 1
          }
            rasList <- na.omit(cbind(r, rSD, studyAreaDT))
            names(rasList) <- c("Mean", "SD", "location")
            locName <- attr(studyAreaDT, "conversionTable")
            dt <- rbindlist(lapply(X = na.omit(unique(studyAreaDT$location)), 
                                   FUN = function(locality){
                                     species <- usefulFuns::substrBoth(strng = names(ras), 
                                                                       howManyCharacters = 4, 
                                                                       fromEnd = FALSE)
                                     message(crayon::blue(paste0("Making table and plot for ", 
                                                                 crayon::magenta(paste(scen, species, YEAR, locality,
                                                                                      collapse = " ")))))
                                     locName <- locName[regionID == locality, regionName]
                                     rawVals <- na.omit(rasList[location == locality, get("Mean")])
                                     CI <- qt(0.975, df = length(rawVals)-1) * 
                                                  sd(rawVals) / 
                                                      sqrt(length(rawVals))
                                     Mean <- rasList[location == locality, mean(get("Mean"), na.rm = TRUE)]
                                     dt <- data.table(scenario = scen,
                                                      species = species,
                                                      location = locality,
                                                      locationName = locName,
                                                      year = YEAR,
                                                      average = Mean,
                                                      lCI = Mean - CI,
                                                      uCI = Mean + CI,
                                                      CI = CI,
                                                      max = rasList[location == locality, max(get("SD"), na.rm = TRUE)],
                                                      std = rasList[location == locality, mean(get("SD"), na.rm = TRUE)])
                                     return(dt)
                                   }))
          return(dt)
        }))
        return(dt)
      }))
      plan("sequential")
      return(plotMaps)
    }))
    saveRDS(plotMaps, file = averageTimePlotTablePath)
  }
  
  # ~~~~~~~~~~~~~~~~~~~ INDIVIDUAL EFFECTS BY POLYGON
  if (makeIndividualEffectsByPolygon){
    if (length(unique(plotMaps$scenario)) == 2)
      cols <- c("red4", "forestgreen") else 
        if (length(unique(plotMaps$scenario)) == 3) {
          cols <- c("steelblue3", "red4", "forestgreen")
        } else {
          cols <- c("steelblue3")
        }
    allBirdsPlots <- lapply(Species, function(BIRD){
      birdTable <- plotMaps[species == BIRD,]
      p <-  ggplot(data = birdTable, aes(x = as.numeric(year), y = average,
                                         group = scenario)) +  
        geom_ribbon(aes(fill = scenario, ymin = (average - std),
                        ymax = (average + std)), alpha = 0.5) +
        scale_fill_manual(values = cols) +
        geom_line(aes(color = scenario)) +
        scale_color_manual(values = cols) +
        scale_x_continuous(limits = c(2011, 2100)) +
        xlab(label = "years") +
        ylab(label = paste0("Averaged direct and indirect ",
                            "effects of climate on bird density")) +
        theme_bw() +
        facet_grid(locationName ~ scenario) + 
        theme(legend.position = "none")
      ggsave(filename = file.path(dataFolder, paste0("averageTimePlot_", 
                                                     BIRD, "PerPolygon.png")), 
             plot = p, device = "png")
      
      return(p)
    })
    names(allBirdsPlots) <- Species
  }

  # ~~~~~~~~~~~~~~~~~~~ INDIVIDUAL EFFECTS GENERAL
  
  if (makeIndividualEffects){
    birdTable <- data.table::copy(plotMaps)
    birdTable[, c("minVal", "maxVal") := list(average-std, average+std)]    
    birdTable <- melt(data = birdTable, id.vars = c("species", "scenario", 
                                                  "locationName", "average", 
                                                  "std", "year"), 
                     measure.vars = c("minVal", "maxVal"))
    birdTable[ , c("averagePols", "sdPols") := list(mean(average), sd(value)), 
              by = c("species", "scenario", "year")]
    cols <- c("steelblue3", "red4", "forestgreen")
    p <-  ggplot(data = birdTable, aes(x = as.numeric(year), y = averagePols,
                                      group = scenario)) +  
      geom_ribbon(aes(fill = scenario, ymin = (averagePols - sdPols),
                      ymax = (averagePols + sdPols)), alpha = 0.5) +
      scale_fill_manual(values = cols) +
      geom_line(aes(color = scenario)) +
      scale_color_manual(values = cols) +
      scale_x_continuous(limits = c(2011, 2100)) +
      xlab(label = "years") +
      ylab(label = "Averaged direct and indirect effects of climate on bird density") +
      theme_bw() +
      facet_grid(species ~ scenario) +
      theme(legend.position = "none")
    
    ggsave(filename = file.path(dataFolder, "averageTimePlot.png"), 
           plot = p, device = "png")
  }
  
  # ~~~~~~~~~~~~~~~~~~~ CUMULATIVE EFFECTS BY POLYGON

  if (useGeneralNWTclass){
    # group polygons by region!!
    studyAreaSF <- sf::st_as_sf(x = shp)
    region <- studyAreaSF$ECOREGION # <~~~~~~~~ HARDCODED FOR shp = ECOREGIONS
    southern <- c(61, 62, 63, 64, 65, 136)
    mountain <- 51
    northern <- c(50, 52, 53, 54, 57)
    central <- c(55, 56, 59, 60, 58)
    region[region %in% southern] <- 1
    region[region %in% mountain] <- 2
    region[region %in% northern] <- 3
    region[region %in% central] <- 4
    studyAreaSF$region <- region
    studyAreaRAS <- fasterize::fasterize(sf = studyAreaSF, 
                                         raster = rasterToMatch,
                                         field = "region")
    # Bring the original classification
    studyAreaDT <- na.omit(convertSAtoDT(studyArea = shp, 
                                         field = field, 
                                         rasterToMatch = rasterToMatch))
    
    convTable <- merge(data.table(regionName = studyAreaSF[[field]],
                                  region = studyAreaSF$region), 
                       attr(studyAreaDT, "conversionTable"))
    names(convTable)[names(convTable) == "regionID"] <- "location"
    convTable[, regionName := NULL]
    plotMaps <- merge(plotMaps, convTable, by = "location")
  } else {
    region <- unique(shp[[field]])
  }
  if (makeCumEffectsByPolygon){
    allBirdsPlots <- lapply(Species, function(BIRD){
        birdTable <- plotMaps[species == BIRD]
        birdTable[, c("minVal", "maxVal") := list(average-std, average+std)]    
        birdTable <- melt(data = birdTable, id.vars = c("species", "scenario",
                                                        "locationName", "average",
                                                        "std", "year"),
                          measure.vars = c("minVal", "maxVal"))
        birdTable[ , c("averagePols", "sdPols") := list(mean(average), sd(value)), 
                   by = c("species", "locationName", "year")]
        maxMin <- birdTable[, c("species", "scenario", "locationName", "year", "variable", "value")]
        maxMin <- dcast(maxMin, formula = species + scenario + locationName + year ~ variable)
        birdTable <- unique(birdTable[, c("variable", "value") := NULL])
        birdTable <- merge(birdTable, maxMin, by =  c("species", "scenario", "locationName", "year"))
        
        p <-  ggplot(data = birdTable, aes(x = as.numeric(year), y = averagePols,
                                           group = locationName)) +
          # geom_ribbon(aes(fill = locationName, ymin = minVal,
          #                 ymax = maxVal), alpha = 0.3) + # THERE IS STILL SMTH WRONG. THE VALUES
          #                 ARE WRONG SOMEHOW
          geom_ribbon(aes(fill = locationName, ymin = (averagePols - sdPols),
                          ymax = (averagePols + sdPols)), alpha = 0.5) +
          # The smaller the alpha, the more transparent
          geom_line(aes(color = locationName)) +
          scale_x_continuous(limits = c(2011, 2100)) +
          xlab(label = "years") +
          ylab(label = paste0("Averaged net effect of climate on ", 
                              BIRD," density")) +
          theme_bw() +
          facet_grid(locationName ~ ., labeller = label_wrap_gen(width = 12)) +
          theme(legend.position = "none")
        fileName <- file.path(dataFolder, paste0("averageTimePlot_", BIRD, 
                                                 "_PerPolygon.png"))
        ggsave(filename = fileName,
               plot = p, device = "png")
        
        return(fileName)
    })
    names(allBirdsPlots) <- Species
    return(allBirdsPlots)
  }
  
  # ~~~~~~~~~~~~~~~~~~~ CUMULATIVE EFFECTS
  
  if (makeCumEffects){
    birdTable <- copy(plotMaps)
    birdTable[, c("minVal", "maxVal") := list(average-std, average+std)]    
    birdTable <- melt(data = birdTable, id.vars = c("species", "scenario",
                                                    "locationName", "average",
                                                    "std", "year"),
                      measure.vars = c("minVal", "maxVal"))
    birdTable[ , c("averagePols", "sdPols") := list(mean(average), sd(value)), 
               by = c("species", "locationName", "year")]
    birdTable[, c("minValCumm", "maxValCumm") := list(averagePols-sdPols, 
                                                     averagePols+sdPols)]
    birdTable <- melt(data = birdTable, id.vars = c("species", "scenario", 
                                                  "year", "averagePols"), 
                     measure.vars = c("minValCumm", "maxValCumm"))
    birdTable[ , c("averagePolsCumm", "sdPolsCumm") := list(mean(averagePols), 
                                                           sd(value)), 
              by = c("species", "year")]
    birdTable <- unique(birdTable, by = c("species", "year")) # Cleanup
    birdTable <- birdTable[, c("averagePols", "variable", "value") := NULL] # Cleanup
    # cols <- c("goldenrod2", "grey40", "darkorchid2")
    
    library(ggplot2)
    p2 <- ggplot(data = birdTable, aes(x = as.numeric(year), y = averagePolsCumm,
                                      group = species)) +  
      geom_ribbon(aes(fill = species, ymin = (averagePolsCumm - sdPolsCumm),
                      ymax = (averagePolsCumm + sdPolsCumm)), alpha = 0.5) +
      # scale_fill_manual(values = cols) +
      geom_line(aes(color = species)) +
      # scale_color_manual(values = cols) +
      scale_x_continuous(limits = c(2011, 2100)) +
      xlab(label = "years") +
      ylab(label = "Averaged cumulative effect of climate on bird density") +
      theme_bw() +
      facet_grid(species ~ .) +
      theme(legend.position = "none")
    ggsave(filename = file.path(dataFolder, 
                                paste0(sp, "AverageTimePlotCumulative.png")), 
           plot = p2, device = "png")
  }
}
