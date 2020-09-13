makeAveragePlotTime <- function(dataFolder, # Where the mean rasters are (effectsRasters)!
                                Species, 
                                scenarios,
                                years,
                                field,
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
    plotMaps <- rbindlist(lapply(scenarios, FUN = function(scen){
      plotMaps <- rbindlist(lapply(Species, FUN = function(sp){
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
            # locName <- attr(studyAreaDT, "conversionTable") # Misplaced
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
      return(plotMaps)
    }))
    saveRDS(plotMaps, file = averageTimePlotTablePath)
  }

  # ~~~~~~~~~~~~~~~~~~~ INDIVIDUAL EFFECTS BY POLYGON

  if (makeIndividualEffectsByPolygon){
    if (length(unique(plotMaps$scenarios)) == 2)
      cols <- c("red4", "forestgreen") else 
        if (length(unique(plotMaps$scenarios)) == 3) {
        } else {
          cols <- c("steelblue3", "red4", "forestgreen")
        }
    cols <- c("steelblue3")
    allBirdsPlots <- lapply(Species, function(BIRD){
      birdTable <- plotMaps[species == BIRD,]
      p <-  ggplot(data = birdTable, aes(x = as.numeric(year), y = average,
                                         group = scenarios)) +  
        geom_ribbon(aes(fill = scenarios, ymin = (average - std),
                        ymax = (average + std)), alpha = 0.5) +
        scale_fill_manual(values = cols) +
        geom_line(aes(color = scenarios)) +
        scale_color_manual(values = cols) +
        scale_x_continuous(limits = c(2011, 2100)) +
        xlab(label = "years") +
        ylab(label = paste0("Averaged direct and indirect ",
                            "effects of climate on bird density")) +
        theme_bw() +
        facet_grid(locationName ~ scenarios) + 
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
    plotMaps[, c("minVal", "maxVal") := list(average-std, average+std)]
    plotMaps <- melt(data = plotMaps, id.vars = c("species", "scenarios", 
                                                  "locationName", "average", 
                                                  "std", "year"), 
                     measure.vars = c("minVal", "maxVal"))
    plotMaps[ , c("averagePols", "sdPols") := list(mean(average), sd(value)), 
              by = c("species", "scenarios", "year")]
    cols <- c("steelblue3", "red4", "forestgreen")
    p <-  ggplot(data = plotMaps, aes(x = as.numeric(year), y = average,
                                      group = scenarios)) +  
      geom_ribbon(aes(fill = scenarios, ymin = (average - std),
                      ymax = (average + std)), alpha = 0.5) +
      scale_fill_manual(values = cols) +
      geom_line(aes(color = scenarios)) +
      scale_color_manual(values = cols) +
      scale_x_continuous(limits = c(2011, 2100)) +
      xlab(label = "years") +
      ylab(label = "Averaged direct and indirect effects of climate on bird density") +
      theme_bw() +
      facet_grid(species ~ scenarios) +
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
  }
  
  if (makeCumEffectsByPolygon){
    allBirdsPlots <- lapply(Species, function(BIRD){
      birdsByRegion <- lapply(unique(region), function(REGION){
        birdTable <- plotMaps[species == BIRD & region == REGION]
        p <-  ggplot(data = birdTable, aes(x = as.numeric(year), y = average,
                                           group = locationName)) +
          geom_ribbon(aes(fill = locationName, ymin = lCI,
                          ymax = uCI), alpha = 0.5) +
          geom_ribbon(aes(fill = locationName, ymin = (average - std),
                          ymax = (average + std)), alpha = 0.3) + 
          # The smaller the alpha, the more transparent
          geom_line(aes(color = locationName)) +
          scale_x_continuous(limits = c(2011, 2100)) +
          xlab(label = "years") +
          ylab(label = paste0("Averaged net effect of climate on ", 
                              BIRD," density")) +
          theme_bw() +
          facet_grid(locationName ~ ., labeller = label_wrap_gen(width = 12)) +
          theme(legend.position = "none")
        fileName <- file.path(dataFolder, paste0("averageTimePlot_", BIRD, "_", 
                                                 REGION,"_PerPolygon.png"))
        ggsave(filename = fileName,
               plot = p, device = "png")
        
        return(fileName)
      })
      return(birdsByRegion)
    })
    names(allBirdsPlots) <- Species
    return(allBirdsPlots)
  }
  
  # ~~~~~~~~~~~~~~~~~~~ CUMULATIVE EFFECTS
  
  if (makeCumEffects){

    plotMaps[, c("minValCumm", "maxValCumm") := list(averagePols-sdPols, 
                                                     averagePols+sdPols)]
    plotMaps <- melt(data = plotMaps, id.vars = c("species", "scenarios", 
                                                  "year", "averagePols"), 
                     measure.vars = c("minValCumm", "maxValCumm"))
    plotMaps[ , c("averagePolsCumm", "sdPolsCumm") := list(mean(averagePols), 
                                                           sd(value)), 
              by = c("species", "year")]
    plotMaps <- unique(plotMaps, by = c("species", "year")) # Cleanup
    plotMaps <- plotMaps[, c("averagePols", "variable", "value") := NULL] # Cleanup
    # cols <- c("goldenrod2", "grey40", "darkorchid2")
    
    library(ggplot2)
    p2 <- ggplot(data = plotMaps, aes(x = as.numeric(year), y = averagePolsCumm,
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
