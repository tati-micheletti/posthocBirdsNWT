plotAbundanceThroughTime <- function(pixelsSummaries, # Where the mean rasters are (effectsRasters)!
                               useFuture,
                               overwrite = FALSE,
                               comparisons,
                               locations,
                               years){
  
  message(crayon::yellow(paste0("Starting plots for ", paste(names(pixelsSummaries), collapse = ", "))))
  if (useFuture) plan("multiprocess", workers = 7) # Hard coded as it is very mem demanding!
  plotMaps <- future_lapply(names(pixelsSummaries), FUN = function(sp){
    tic(paste0("Time elapsed for ", sp))
    tb <- qs::qread(pixelsSummaries[[sp]])
    factorialRasters <- lapply(names(comparisons), FUN = function(eachComparison){
      individualPlotTablePath <- file.path(Paths$outputPath, paste("timePlotTable", eachComparison, 
                                                            sp, sep = "_"))
      statsEachComparisonPath <- file.path(Paths$outputPath, paste("timePlotStats", eachComparison, 
                                                              sp, sep = "_"))
      if (!all(!file.exists(paste0(individualPlotTablePath, ".qs")),
               !file.exists(paste0(statsEachComparisonPath, ".qs")))){
        
        allComparisons <- data.table(expand.grid(comparisons))
        allComparisons[, allComps := paste0(vegetation, fire, "_", climate)]
        allComparisons <- allComparisons$allComps
        climateGroupNames <- sort(allComparisons[grep(comparisons[[eachComparison]][1], 
                                                      x = allComparisons)])
        nonclimateGroupNames <- sort(allComparisons[grep(comparisons[[eachComparison]][2], 
                                                         x = allComparisons)])
        # Difference for the scenarios
        lapply(X = seq_along(climateGroupNames), function(index){
          climateGroup <- climateGroupNames[index]
          nonclimateGroup <- nonclimateGroupNames[index]
          tb[, paste0(eachComparison, index) := get(climateGroup) - get(nonclimateGroup)]
        })
        compNames <- usefulFuns::grepMulti(names(tb), patterns = eachComparison)
        compCols <- c("location", "Year", compNames)
        DT <- tb[, ..compCols]
        DT <- melt(DT, id.vars = setdiff(compCols, compNames),
                   measure.vars = compNames)
        gc()
        DT[, variable := NULL]
        setattr(DT, "species", sp)
        setattr(DT, "comparison", eachComparison)
        # Stats by location
        statsEachComparison <- rbindlist(lapply(locations, FUN = function(loc){
          locDT <- DT[DT[location == loc, .I[sample(.N, 100)], by = Year][["V1"]]]
          tend <-lm(value ~ Year, data = locDT)
          require(stats)
          coeff <- coefficients(tend)
          Fstats <- summary(tend)$fstatistic
          names(Fstats) <- NULL
          P <- pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F)
          pValue <- ifelse(P < 0.05, " (significant)", "(non-significant)")
          coefX <- round(coeff[2],1)
          coefY <- round(coeff[1],1)
          statsDT <- data.table(species = sp,
                                comparison = eachComparison,
                                location = loc,
                                coefX = coefX,
                                coefY = coefY,
                                pValue = P,
                                labelText = c(paste0("Location ", loc, ": y = ", 
                                                    ifelse(coefX < 10000, coefX, 
                                                           formatC(coefX, format = "e", digits = 2)),
                                                    "x + ", ifelse(coefY < 10000, coefY,
                                                                   formatC(coefY, format = "e", 
                                                                           digits = 2)), pValue)))
          return(statsDT)
        }))
        # Save both things
        qs::qsave(DT, paste0(individualPlotTablePath, ".qs")) 
        qs::qsave(statsEachComparison, paste0(statsEachComparisonPath, ".qs"))
        toc()
        
# tic("Plotting") # Crashes badly; need to do later somehow
#         p <- ggplot(DT, aes(x = Year, y = value)) +
#           geom_hex(bins = 70) +
#           scale_fill_continuous(type = "viridis") +
#           theme_bw() +
#           facet_grid(. ~ location)
#         p
# toc()
        return(list(individualPlotTable = individualPlotTablePath,
                    statsEachComparison = statsEachComparisonPath))
      } else {
        return(list(individualPlotTable = individualPlotTablePath,
                    statsEachComparison = statsEachComparisonPath))
      }
        })
    names(factorialRasters) <- names(comparisons)
    toc()
    return(factorialRasters)
  })
  names(plotMaps) <- names(pixelsSummaries)
  return(plotMaps)
}
  #   # plotMaps <- rbindlist(lapply(scenarios, FUN = function(scen){ # CAN ONLY USE 22 SPECIES AT A TIME, MAYBE LESS!
  #     # if (useFuture) plan("multiprocess", workers = length(Species)/3) # <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ future_lapply
  # 
  # 
  # tendS <-lm(fireSize ~ year, data = fireSize)
  # require(stats)
  # coeffS <- coefficients(tendS)
  # Fstats <- summary(tendS)$fstatistic
  # names(Fstats) <- NULL
  # pValueS <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, " \n(significant)", " \n(non-significant)")
  # coefXA <- round(coeff[2],1)
  # coefYA <- round(coeff[1],1)
  # coefXF <- round(coeffF[2],1)
  # coefYF <- round(coeffF[1],1)
  # coefXS <- round(coeffS[2],1)
  # coefYS <- round(coeffS[1],1)
  # 
  # # New facet label names for dose variable
  # replacementNames <- c(paste0("Area burned: ",
  #                              "y = ", ifelse(coefXA < 10000, coefXA, formatC(coefXA, format = "e", digits = 2)),
  #                              "x + ", ifelse(coefYA < 10000, coefYA, formatC(coefYA, format = "e", digits = 2)), pValueA),
  #                       paste0("No fires: ",
  #                              "y = ", ifelse(coefXF < 10000, coefXF, formatC(coefXF, format = "e", digits = 2)),
  #                              "x + ", ifelse(coefYF < 10000, coefYF, formatC(coefYF, format = "e", digits = 2)), pValueF),
  #                       paste0("Mean fire size: ",
  #                              "y = ", ifelse(coefXS < 10000, coefXS, formatC(coefXS, format = "e", digits = 2)),
  #                              "x + ", ifelse(coefYS < 10000, coefYS, formatC(coefYS, format = "e", digits = 2)), pValueS))
  # names(replacementNames) <- c("area_burned", "number_fires", "fire_size")
  # 
  # p1 <- ggplot2::ggplot(data = dt[var == "area_burned",], aes(x = year, y = val)) +
  #   geom_point(colour = "grey70") +
  #   stat_smooth(method = "lm", color = "darkred", fill = "red") +
  #   facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
  #   theme(legend.position = "none",
  #         strip.text.y = element_text(size = 9, face = "bold"),
  #         axis.title.x = element_blank(),
  #         axis.text.x = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm")) +
  #   coord_cartesian(ylim = c(100, 1500)) +
  #   labs(y = "ha x 10^3")
  # p2 <- ggplot(data = dt[var == "number_fires",], aes(x = year, y = val, colour = "blue")) +
  #   geom_point(colour = "grey70") +
  #   stat_smooth(method = "lm", fill = "blue", color = "darkblue") +
  #   facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
  #   theme(legend.position = "none",
  #         strip.text.y = element_text(size = 9, face = "bold"),
  #         plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm"),
  #         axis.title.x = element_blank(),
  #         axis.text.x = element_blank(),
  #         axis.ticks.x = element_blank()) +
  #   coord_cartesian(ylim = c(0, 500)) +
  #   ylab(label = "no. of fires")
  # p3 <- ggplot2::ggplot(data = dt[var == "fire_size",], aes(x = year, y = val)) +
  #   geom_point(colour = "grey70") +
  #   stat_smooth(method = "lm", color = "orange", fill = "orange") +
  #   facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
  #   theme(legend.position = "none",
  #         strip.text.y = element_text(size = 9, face = "bold"),
  #         plot.margin = unit(c(-0.01, 0.2, 0.2, 0.2), "cm")) +
  #   coord_cartesian(ylim = c(100, 1500)) +
  #   labs(y = "ha x 10")
  # 
  # p <- gridExtra::grid.arrange(p1, p2, p3, ncol=1,
  #                              top = grid::textGrob(typeSim, gp = grid::gpar(fontsize = 12)))
  # 
  # ggsave(fileName, plot = p, width = 11, height = 8)
  # 
  # 
  # 
  # 
  # # ~~~~~~~~~~~~~~~~~~~ INDIVIDUAL EFFECTS BY POLYGON
  # browser()
  # if (makeIndividualEffectsByPolygon){
  #   if (length(unique(plotMaps$scenario)) == 2)
  #     cols <- c("red4", "forestgreen") else 
  #       if (length(unique(plotMaps$scenario)) == 3) {
  #         cols <- c("steelblue3", "red4", "forestgreen")
  #       } else {
  #         cols <- c("steelblue3")
  #       }
  #   allBirdsPlots <- lapply(Species, function(BIRD){
  #     birdTable <- plotMaps[species == BIRD,]
  #     p <-  ggplot(data = birdTable, aes(x = as.numeric(year), y = average,
  #                                        group = scenario)) +  
  #       geom_ribbon(aes(fill = scenario, ymin = (average - std),
  #                       ymax = (average + std)), alpha = 0.5) +
  #       scale_fill_manual(values = cols) +
  #       geom_line(aes(color = scenario)) +
  #       scale_color_manual(values = cols) +
  #       scale_x_continuous(limits = c(2011, 2100)) +
  #       xlab(label = "years") +
  #       ylab(label = paste0("Averaged direct and indirect ",
  #                           "effects of climate on bird density")) +
  #       theme_bw() +
  #       facet_grid(locationName ~ scenario) + 
  #       theme(legend.position = "none")
  #     ggsave(filename = file.path(dataFolder, paste0("averageTimePlot_", 
  #                                                    BIRD, "PerPolygon.png")), 
  #            plot = p, device = "png")
  #     
  #     return(p)
  #   })
  #   names(allBirdsPlots) <- Species
  # }
  # 
  # # ~~~~~~~~~~~~~~~~~~~ INDIVIDUAL EFFECTS GENERAL
  # 
  # if (makeIndividualEffects){
  #   birdTable <- data.table::copy(plotMaps)
  #   birdTable[, c("minVal", "maxVal") := list(average-std, average+std)]    
  #   birdTable <- melt(data = birdTable, id.vars = c("species", "scenario", 
  #                                                   "locationName", "average", 
  #                                                   "std", "year"), 
  #                     measure.vars = c("minVal", "maxVal"))
  #   birdTable[ , c("averagePols", "sdPols") := list(mean(average), sd(value)), 
  #              by = c("species", "scenario", "year")]
  #   cols <- c("steelblue3", "red4", "forestgreen")
  #   p <-  ggplot(data = birdTable, aes(x = as.numeric(year), y = averagePols,
  #                                      group = scenario)) +  
  #     geom_ribbon(aes(fill = scenario, ymin = (averagePols - sdPols),
  #                     ymax = (averagePols + sdPols)), alpha = 0.5) +
  #     scale_fill_manual(values = cols) +
  #     geom_line(aes(color = scenario)) +
  #     scale_color_manual(values = cols) +
  #     scale_x_continuous(limits = c(2011, 2100)) +
  #     xlab(label = "years") +
  #     ylab(label = "Averaged direct and indirect effects of climate on bird density") +
  #     theme_bw() +
  #     facet_grid(species ~ scenario) +
  #     theme(legend.position = "none")
  #   
  #   ggsave(filename = file.path(dataFolder, "averageTimePlot.png"), 
  #          plot = p, device = "png")
  # }
  # 
  # # ~~~~~~~~~~~~~~~~~~~ CUMULATIVE EFFECTS BY POLYGON
  # 
  # if (useGeneralNWTclass){
  #   # group polygons by region!!
  #   studyAreaSF <- sf::st_as_sf(x = shp)
  #   region <- studyAreaSF$ECOREGION # <~~~~~~~~ HARDCODED FOR shp = ECOREGIONS
  #   southern <- c(61, 62, 63, 64, 65, 136)
  #   mountain <- 51
  #   northern <- c(50, 52, 53, 54, 57)
  #   central <- c(55, 56, 59, 60, 58)
  #   region[region %in% southern] <- 1
  #   region[region %in% mountain] <- 2
  #   region[region %in% northern] <- 3
  #   region[region %in% central] <- 4
  #   studyAreaSF$region <- region
  #   studyAreaRAS <- fasterize::fasterize(sf = studyAreaSF, 
  #                                        raster = rasterToMatch,
  #                                        field = "region")
  #   # Bring the original classification
  #   studyAreaDT <- na.omit(convertSAtoDT(studyArea = shp, 
  #                                        field = field, 
  #                                        rasterToMatch = rasterToMatch))
  #   
  #   convTable <- merge(data.table(regionName = studyAreaSF[[field]],
  #                                 region = studyAreaSF$region), 
  #                      attr(studyAreaDT, "conversionTable"))
  #   names(convTable)[names(convTable) == "regionID"] <- "location"
  #   convTable[, regionName := NULL]
  #   plotMaps <- merge(plotMaps, convTable, by = "location")
  # } else {
  #   region <- unique(shp[[field]])
  # }
  # if (makeCumEffectsByPolygon){
  #   allBirdsPlots <- lapply(Species, function(BIRD){
  #     birdTable <- plotMaps[species == BIRD]
  #     birdTable[, c("minVal", "maxVal") := list(average-std, average+std)]    
  #     birdTable <- melt(data = birdTable, id.vars = c("species", "scenario",
  #                                                     "locationName", "average",
  #                                                     "std", "year"),
  #                       measure.vars = c("minVal", "maxVal"))
  #     birdTable[ , c("averagePols", "sdPols") := list(mean(average), sd(value)), 
  #                by = c("species", "locationName", "year")]
  #     maxMin <- birdTable[, c("species", "scenario", "locationName", "year", "variable", "value")]
  #     maxMin <- dcast(maxMin, formula = species + scenario + locationName + year ~ variable)
  #     birdTable <- unique(birdTable[, c("variable", "value") := NULL])
  #     birdTable <- merge(birdTable, maxMin, by =  c("species", "scenario", "locationName", "year"))
  #     
  #     p <-  ggplot(data = birdTable, aes(x = as.numeric(year), y = averagePols,
  #                                        group = locationName)) +
  #       # geom_ribbon(aes(fill = locationName, ymin = minVal,
  #       #                 ymax = maxVal), alpha = 0.3) + # THERE IS STILL SMTH WRONG. THE VALUES
  #       #                 ARE WRONG SOMEHOW
  #       geom_ribbon(aes(fill = locationName, ymin = (averagePols - sdPols),
  #                       ymax = (averagePols + sdPols)), alpha = 0.5) +
  #       # The smaller the alpha, the more transparent
  #       geom_line(aes(color = locationName)) +
  #       scale_x_continuous(limits = c(2011, 2100)) +
  #       xlab(label = "years") +
  #       ylab(label = paste0("Averaged net effect of climate on ", 
  #                           BIRD," density")) +
  #       theme_bw() +
  #       facet_grid(locationName ~ ., labeller = label_wrap_gen(width = 12)) +
  #       theme(legend.position = "none")
  #     fileName <- file.path(dataFolder, paste0("averageTimePlot_", BIRD, 
  #                                              "_PerPolygon.png"))
  #     ggsave(filename = fileName,
  #            plot = p, device = "png")
  #     
  #     return(fileName)
  #   })
  #   names(allBirdsPlots) <- Species
  #   return(allBirdsPlots)
  # }
  # 
  # # ~~~~~~~~~~~~~~~~~~~ CUMULATIVE EFFECTS
  # 
  # if (makeCumEffects){
  #   birdTable <- copy(plotMaps)
  #   birdTable[, c("minVal", "maxVal") := list(average-std, average+std)]    
  #   birdTable <- melt(data = birdTable, id.vars = c("species", "scenario",
  #                                                   "locationName", "average",
  #                                                   "std", "year"),
  #                     measure.vars = c("minVal", "maxVal"))
  #   birdTable[ , c("averagePols", "sdPols") := list(mean(average), sd(value)), 
  #              by = c("species", "locationName", "year")]
  #   birdTable[, c("minValCumm", "maxValCumm") := list(averagePols-sdPols, 
  #                                                     averagePols+sdPols)]
  #   birdTable <- melt(data = birdTable, id.vars = c("species", "scenario", 
  #                                                   "year", "averagePols"), 
  #                     measure.vars = c("minValCumm", "maxValCumm"))
  #   birdTable[ , c("averagePolsCumm", "sdPolsCumm") := list(mean(averagePols), 
  #                                                           sd(value)), 
  #              by = c("species", "year")]
  #   birdTable <- unique(birdTable, by = c("species", "year")) # Cleanup
  #   birdTable <- birdTable[, c("averagePols", "variable", "value") := NULL] # Cleanup
  #   # cols <- c("goldenrod2", "grey40", "darkorchid2")
  #   
  #   library(ggplot2)
  #   p2 <- ggplot(data = birdTable, aes(x = as.numeric(year), y = averagePolsCumm,
  #                                      group = species)) +  
  #     geom_ribbon(aes(fill = species, ymin = (averagePolsCumm - sdPolsCumm),
  #                     ymax = (averagePolsCumm + sdPolsCumm)), alpha = 0.5) +
  #     # scale_fill_manual(values = cols) +
  #     geom_line(aes(color = species)) +
  #     # scale_color_manual(values = cols) +
  #     scale_x_continuous(limits = c(2011, 2100)) +
  #     xlab(label = "years") +
  #     ylab(label = "Averaged cumulative effect of climate on bird density") +
  #     theme_bw() +
  #     facet_grid(species ~ .) +
  #     theme(legend.position = "none")
  #   ggsave(filename = file.path(dataFolder, 
  #                               paste0(sp, "AverageTimePlotCumulative.png")), 
  #          plot = p2, device = "png")
  # }

