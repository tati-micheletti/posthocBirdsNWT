calcColonizationList <- function(listOfRasters, 
                                 species,
                                 years,
                                 comparisons,
                                 outputFolder,
                                 overwrite,
                                 useFuture){
  browser()
  # This also for climate and no climate
  # Lapply through birds and percentToDiscard
  allBirds <- lapply(species, function(sp){
    allComparisonsPerBird <- lapply(comparisons, function(compare){
      # We need to return 2 things: 
      # 1) the difference between 2011 and 2100; 
      # 2) the difference of 1) between the scenarios fS (rasterT1) - SCFM (rasterT0)
      
    })
  })
  
  HOLA_0.5_fS <- calcColonization(rasT0Path = "~/projects/NWT/outputs/14AUG20/LandR.CS_fS/run1/birdPredictionsV6a/run1_LandR.CS_fSpredictedHOLAYear2011.tif", 
                               rasT1Path = "~/projects/NWT/outputs/14AUG20/LandR.CS_fS/run1/birdPredictionsV6a/run1_LandR.CS_fSpredictedHOLAYear2100.tif", 
                               percentToDiscard = 0.5)
  
  HOLA_0.5_SCFM <- calcColonization(rasT0Path = "~/projects/NWT/outputs/14AUG20/LandR_SCFM/run1/birdPredictionsV4/run1_LandR_SCFMpredictedHOLAYear2011.tif", 
                                  rasT1Path = "~/projects/NWT/outputs/14AUG20/LandR_SCFM/run1/birdPredictionsV4/run1_LandR_SCFMpredictedHOLAYear2100.tif", 
                                  percentToDiscard = 0.5)
  pal <- colorRampPalette(c("red","lightyellow","green"))
  plot(HOLA_0.5_SCFM, col = pal(5), main = "HOLA with percentToDiscard = 0.5") #plot with defined breaks
  tb_0.5 <- table(HOLA_0.5_SCFM[])
  DT_0.5_SCFM <- data.table(bird = "HOLA",
                       percentToDiscard = 0.5,
                       # extirpation = tb_0.5["-1"], 
                       # colonization = tb_0.5["1"], 
                       # noChange = tb_0.5["0"],
                       percExtirpation = tb_0.5["-1"]/sum(tb_0.5),
                       percColonization = tb_0.5["1"]/sum(tb_0.5),
                       percNoChange = tb_0.5["0"]/sum(tb_0.5))
  return(list(colRasters = colRasters, colTable = colTable))
}

calcColonization <- function(rasT0Path, rasT1Path, percentToDiscard){
  rasT0 <- raster::raster(rasT0Path)
  rasT1 <- raster::raster(rasT1Path)
  rasT0 <- .presenceAbsenceRas(rasT0, percentToDiscard)
  rasT1 <- .presenceAbsenceRas(rasT1, percentToDiscard)
  rasCol <- rasT1 - rasT0
  return(rasCol)
}

.presenceAbsenceRas <- function(ras, percentToDiscard = 0.3){
  CSdt <- data.table::data.table(pixelID = 1:ncell(ras), 
                                 val = getValues(ras))
  CSdt <- na.omit(CSdt)
  data.table::setkey(CSdt, val)
  CSdt[, CUM := cumsum(val)]
  CSdt[, CUMstd := CUM/sum(val)]
  CSdt[, PA := CUMstd > percentToDiscard]
  BIRDpres <- raster(ras)
  BIRDpres[CSdt[PA == TRUE, pixelID]] <- 1
  BIRDpres[CSdt[PA == FALSE, pixelID]] <- 0
  return(BIRDpres)
}
