convertSAtoDT <- function(studyArea, field, rasterToMatch){
  # Fasterize studyArea, make a data.table and add the attribute of the name
  studyAreaSF <- sf::st_as_sf(x = studyArea)
  studyAreaSF$newID <- as.numeric(as.factor(studyAreaSF[[field]]))
    conversionTable <- data.table(regionID = studyAreaSF$newID, 
                                  regionName = studyAreaSF[[field]])
  studyAreaSF <- fasterize::fasterize(sf = studyAreaSF, 
                                      raster = rasterToMatch,
                                      field = "newID")
  studyAreaDT <- data.table::data.table(getValues(studyAreaSF))
  names(studyAreaDT) <- "location"
  attr(studyAreaDT, 'conversionTable') <- conversionTable
  return(studyAreaDT)
}