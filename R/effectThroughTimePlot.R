effectThroughTimePlot <- function(averageTimePlot, howManyPoints = 5000,
                                  plotFilePath,
                                  typePlot){
  speciesPlots <- lapply(names(averageTimePlot), function(sp){
    plotFileName <- file.path(plotFilePath, paste0(typePlot, "_", sp, ".png"))
    if (!file.exists(plotFileName)){
      tic(paste0("Making plots for ", sp))
      comps <- names(averageTimePlot[[sp]])
      effectPlots <- lapply(comps, function(eachComparison){
        tic(paste0("Plots for ", sp, " for ", eachComparison))
        DT <- qs::qread(averageTimePlot[[sp]][[eachComparison]][["individualPlotTable"]])
        plotDT <- na.omit(DT[DT[, .I[sample(.N, howManyPoints)], by = c("Year", "location")][["V1"]]])
        
        pal <- c("#FF9999", "#66CCFF", "#FFFF66", "#99FF99", "#9999FF")
        
        midPoint <- comps[round(length(comps)/2, 0)]
        if (eachComparison != comps[length(comps)]){
          p <- ggplot(plotDT, aes(x = Year, y = value, col = as.factor(location))) +
            geom_jitter(size = 0.5, alpha = 0.3) +
            scale_color_manual(values = pal) +
            geom_hline(yintercept = 0, color = "grey50") +
            stat_smooth(method = "lm", color = "black", 
                        fill = "red") +
            facet_grid(. ~ location) +
            coord_cartesian(ylim = c(-0.5, 0.5)) +
            theme(legend.position = "none",
                  strip.text.x = element_text(size = 9, face = "bold"),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.title.y = element_blank(),
                  plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm"))
        } else {
          p <- ggplot(plotDT, aes(x = Year, y = value, col = as.factor(location))) +
            geom_jitter(size = 0.5, alpha = 0.3) +
            scale_color_manual(values = pal) +
            geom_hline(yintercept = 0, color = "grey50") +
            stat_smooth(method = "lm", color = "black", 
                        fill = "red") +
            facet_grid(. ~ location) +
            theme(legend.position = "none",
                  axis.title.y = element_blank(),
                  strip.text.x = element_text(size = 9, face = "bold"),
                  plot.margin = unit(c(-0.01, 0.2, 0.2, 0.2), "cm")) + 
            coord_cartesian(ylim = c(-0.5, 0.5))
        }
        toc()
        return(p)
      })
      toc()
      # Combine the plots
      if (length(effectPlots) > 1){
        combinedPlots <- gridExtra::grid.arrange(effectPlots[[1]], effectPlots[[2]], effectPlots[[3]], ncol = 1,
                                                 top = grid::textGrob(sp, gp = grid::gpar(fontsize = 12)))
      } else {
        if (length(effectPlots) == 1){
          combinedPlots <- gridExtra::grid.arrange(effectPlots[[1]], ncol = 1,
                                                   top = grid::textGrob(sp, gp = grid::gpar(fontsize = 12)))
        } else {
          stop("Either 1 or 3 plots must nbe supplied")
        }
      }
      ggsave(plotFileName, plot = combinedPlots, width = 11, height = 8, device = "png")
    }
    return(plotFileName)
  })
  names(speciesPlots) <- names(averageTimePlot)
return(speciesPlots)
}
