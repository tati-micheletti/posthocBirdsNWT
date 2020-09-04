makeAveragePlotTime <- function(dataFolder, # Where the mean rasters are (effectsRasters)!
                                Species, 
                                scenarios,
                                years,
                                field,
                                rasterToMatch,
                                shp = NULL, 
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
            dt <- rbindlist(lapply(X = na.omit(unique(studyAreaDT$location)), 
                                   FUN = function(locality){
                                     species <- usefulFuns::substrBoth(strng = names(ras), 
                                                                       howManyCharacters = 4, 
                                                                       fromEnd = FALSE)
                                     message(crayon::blue(paste0("Making table and plot for ", 
                                                                 crayon::magenta(paste(scen, species, YEAR, locality,
                                                                                      collapse = " ")))))
                                     locName <- attr(studyAreaDT, "conversionTable") 
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
                                                      mean = Mean,
                                                      lCI = Mean - CI,
                                                      uCI = Mean + CI,
                                                      CI = CI,
                                                      max = rasList[location == locality, max(get("SD"), na.rm = TRUE)],
                                                      sd = rasList[location == locality, mean(get("SD"), na.rm = TRUE)])
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

  # ~~~~~~~~~~~~~~~~~~~ CUMULATIVE EFFECTS BY POLYGON

  # group polygons by region!!
studyAreaSF <- sf::st_as_sf(x = shp)
region <- studyAreaSF$ECOREGION
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

convTable <- merge(data.table(regionName = studyAreaSF$REGION_NAM,
                              region = studyAreaSF$region), 
                   attr(studyAreaDT, "conversionTable"))
names(convTable)[names(convTable) == "regionID"] <- "location"
convTable[, regionName := NULL]
plotMaps <- merge(plotMaps, convTable, by = "location")

  allBirdsPlots <- lapply(Species, function(BIRD){
    birdsByRegion <- lapply(unique(region), function(REGION){
      birdTable <- plotMaps[species == BIRD & region == REGION]
      p <-  ggplot(data = birdTable, aes(x = as.numeric(year), y = mean,
                                         group = locationName)) +
        geom_ribbon(aes(fill = locationName, ymin = lCI,
                        ymax = uCI), alpha = 0.5) +
        geom_ribbon(aes(fill = locationName, ymin = (mean - sd),
                        ymax = (mean + sd)), alpha = 0.3) + # The smaller the alpha, the more transparent
        # scale_fill_manual(values = cols) +
        geom_line(aes(color = locationName)) +
        # scale_color_manual(values = cols) +
        scale_x_continuous(limits = c(2011, 2100)) +
        xlab(label = "years") +
        ylab(label = paste0("Averaged net effect of climate on ", BIRD," density")) +
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
