library(OrgMassSpecR)
rm(list = ls())

# Read in metadata and spectral data

spec <- read.csv("Spectral File - Unknowns.csv", stringsAsFactors = FALSE)
meta <- read.csv("MetaData File - Unknowns.csv", stringsAsFactors = FALSE)

DrawSpectrum <- function(s, xMin = 40, xMax) {

  s$percentIntensity <- with(s, intensity / max(intensity) * 100)
  plotData <-s[s$mz >= xMin & s$mz <= xMax, ]

  pushViewport(plotViewport(c(3.75, 3.5, 1.5, 1)))
    pushViewport(dataViewport(xscale = c(xMin, xMax), yscale = c(0, 110)))
      grid.rect()
      prettyTck <- pretty(plotData$mz, n = 10)
      xTck <- prettyTck[prettyTck >= xMin & prettyTck <= xMax]
      grid.xaxis(at = xTck)
      grid.yaxis(at = c(0, 25, 50, 75, 100))
      grid.segments(plotData$mz, 
        plotData$percentIntensity,
        plotData$mz, 
        rep(0, length(plotData$intensity)),
        default.units = "native", 
        gp = gpar(lwd = 0.75))
      grid.text("intensity (%)", x = unit(-3.2, "lines"), rot = 90)
      grid.text("m/z", y = unit(-2.5, "lines"))
      # Automatic display of values every 25 units
      plotSegments <- cut(plotData$mz, breaks = xMax/25)
      plotData <- cbind(plotData, plotSegments)

      for(i in 1:length(unique(plotSegments))) {
        segmentTmp <- plotData[plotData$plotSegments == unique(plotSegments)[i], ]
        maxPeak <- segmentTmp[segmentTmp$percentIntensity == max(segmentTmp$percentIntensity, na.rm = TRUE), ]
        if(maxPeak$percentIntensity[1] >= 5) {
          grid.text(maxPeak$mz,
            x = maxPeak$mz,
            y = maxPeak$percentIntensity + 5,
            default.units = "native",
            gp = gpar(col = "blue"))
        }
      }     
  popViewport(2)
}
    
# Report function
LibraryReport <- function(spectra = spec, 
  metadata = meta,
  pdfFile = "SpecLibBaja2022.pdf", 
  pdfTitle = "SpecLibBaja2022 Library") {
  
  require(grid)
  require(png)
  
  pdf(file = pdfFile, width = 10.5, height = 8, title = pdfTitle, paper = "usr")
  
  # Title Page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 1, heights = unit(c(0.1, 0.2, 0.5, 0.2), "npc"))))
    pushViewport(viewport(layout.pos.row = 1))
      grid.text("SpecLibBaja2022 Mass Spectral Library", y = 0.5, gp = gpar(cex = 1.25))
      grid.lines(x = unit(c(0,1), "npc"), y = unit(c(0,0), "npc"))
    popViewport()
    pushViewport(viewport(layout.pos.row = 2))
      grid.text("Assessing Marine Endocrine Disrupting Chemicals in the Critically\nEndangered California Condor: Implications for Reintroduction to Coastal Environments", y = 0.5, gp = gpar(cex = 1.5))
    popViewport()
    pushViewport(viewport(layout.pos.row = 3))
      grid.text("Authors: Margaret E. Stack, Jennifer M. Cossaboon, Christopher W. Tubbs, L. Ignacio Vilchis, Rachel G. Felton,\nJade L. Johnson, Kerri Danil, Gisela Heckel, Eunha Hoh, and Nathan G. Dodder", y = 0.9, gp = gpar(cex = 1.25))
      grid.text("Mass spectra of unknown compounds detected in marine mammals\nfrom the Gulf of California, Baja California, Mexico", y = 0.6, gp = gpar(cex = 1.25))
      grid.text("Web Reference: http://OrgMassSpec.github.io", y = 0.3, gp = gpar(cex = 1.25))
    popViewport()
    pushViewport(viewport(layout.pos.row = 4))
      session.info <- sessionInfo()
      grid.text(paste("Prepared:", Sys.time()), y = 0.8)
      grid.text(paste("SpecLibBaja2022"), y = 0.65)
      #grid.text(paste("SpecLibUnknown2017 version", session.info$otherPkgs$SpecLibUnknown2017$Version), y = 0.65)
      grid.text(paste("OrgMassSpecR version", session.info$otherPkgs$OrgMassSpecR$Version), y = 0.5)
      grid.text(session.info$R.version$version.string, y = 0.2)
    popViewport()
  popViewport()
  
  # Make page numbers
  metadata$Page <- 1:nrow(metadata)

  # Page
  DrawPage <- function(currentCompound) {
    
    currentSpectrum <- spectra[spectra$filename == currentCompound, ]
    currentMetadata <- metadata[metadata$CompoundName == currentCompound, ]
    message("Making spectrum for ", currentMetadata$CompoundName)
    
    grid.newpage()
    spec.layout <- grid.layout(nrow = 5, ncol = 1, heights = unit(c(0.05, 0.1, 0.55, 0.25, 0.05), "npc"))
    pushViewport(viewport(layout = spec.layout))
    
      #---------- Compound Name and Class (Page Top) ----------
      pushViewport(viewport(layout.pos.row = 1))
        grid.text(paste("Name:", currentMetadata$CompoundName), x = 0, y = 0.5, just = c("left", "center"), gp = gpar(cex = 1.25))
        grid.text(paste("Class:", currentMetadata$StructuralClass), x = 0.67, y = 0.5, just = c("left", "center"), gp = gpar(cex = 1.25))
        grid.lines(x = unit(c(0,1), "npc"), y = unit(c(0,0), "npc"))
      popViewport()  

      #---------- Compound Info ----------
      pushViewport(viewport(layout.pos.row = 2))
        grid.text("Matrix: Marine Mammal Blubber", x = 0, y = 0.75, hjust = 0)
        grid.text(paste("Species detected in: ", currentMetadata$SpeciesDetectedIn, sep = ""), x = 0, y = 0.5, hjust = 0)                             
        grid.text("Instrument: GCxGC-TOF, EI Mode", x = 0.43, y = 0.75, hjust = 0)
        grid.text(paste("1D RT, 2D RT (s): ", currentMetadata$RetentionTimes, sep = ""), x = 0.43, y = 0.5, hjust = 0)   
    
        if(!is.na(currentMetadata$Comment)) {
          grid.text(paste("Comment:", currentMetadata$Comment ), x = 0, y = 0, hjust = 0)
        } 
      popViewport()
    
      # ---------- Draw spectrum ----------
      pushViewport(viewport(layout.pos.row = 3))
        if(nrow(currentSpectrum) > 0) {
          DrawSpectrum(s = currentSpectrum, xMin = 40, xMax = currentMetadata$AxisMax)
        } else {
          grid.text("Missing Spectrum")
        }
      popViewport()

      #---------- Write filename and page number ----------
      pushViewport(viewport(layout.pos.row = 5))
        grid.text(paste("Page: ", currentMetadata$Page + 1, sep = ""),
              x = 1, just = c("right", "top"), gp = gpar(col = "dark grey"))
      popViewport(2)
  }
  
  sapply(metadata$CompoundName, DrawPage)
  graphics.off()
  
}

LibraryReport()
