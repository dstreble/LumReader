#' Function to plot a Material
#'
#' This function plots the selected Material.
#'
#' @param object
#'  \code{\linkS4class{Material}} to plot
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @import lattice gridExtra plotly
#'
#' @export plot_Material

plot_Material <- function(
  object

){
  if (missing(object)){
    stop("[plot_Material] Error: Input 'object' is missing.")
  }else if (!is(object,"Material")){
    stop("[plot_Material] Error: Input 'object' is not of type 'Material'.")
  }


  name <- object@name

  # TL
  description.TL <- object@description.TL

  TL <- object@TL

  TL.wavelength <- TL[,1]
  TL.temperature <- TL[,2]
  TL.signal <- TL[,3]

  TL.x <- unique(TL.wavelength)
  TL.y <- unique(TL.temperature)
  TL.z <- matrix(data=TL.signal,
              nrow = length(TL.x),
              ncol = length(TL.y),
              byrow = TRUE)

  # OSL
  description.OSL <- object@description.OSL

  OSL <- object@OSL

  OSL.wavelength <- OSL[,1]
  OSL.color <- OSL[,2]
  OSL.signal <- OSL[,3]

  OSL.x <- unique(OSL.wavelength)
  OSL.y <- unique(OSL.color)
  OSL.z <- matrix(data=OSL.signal,
                  nrow = length(OSL.x),
                  ncol = length(OSL.y),
                  byrow = TRUE)

  # contour plot
  #TL
  TL.levelplot <- levelplot(x= TL.z,
                            row.values=TL.x,
                            column.values=TL.y,
                            xlab="Emission wavelength [nm]",
                            ylab="Temperature [\u00b0C]",
                            main=paste("Intensity of the TL emission of", name, "[u.a]"),
                            cuts=39,
                            col.regions=rev(heat.colors(n = 40,alpha = 1)),
                            colorkey=TRUE)

  # OSL
  OSL.levelplot <- levelplot(x= OSL.z,
                             row.values=OSL.x,
                             column.values=OSL.y,
                             xlab="Emission wavelength [nm]",
                             ylab="Stimulation wavelength [nm]",
                             main=paste("Intensity of the OSL emission of", name, "[u.a]"),
                             cuts=39,
                             col.regions=rev(terrain.colors(n = 40,alpha = 1)),
                             colorkey=TRUE)

  grid.arrange(TL.levelplot, OSL.levelplot, nrow=2, ncol=1, respect=FALSE)

  #####################################
  #Plotly
  # TL
  TL.3D <- plot_ly(x=TL.x,
                   y=TL.y,
                   z=TL.z,
                   type = "surface")

  TL.3D.title <- paste("Intensity of the TL emission of", name, "[u.a]")

  TL.3D.scene <- list(xaxis=list(title="Emission wavelength [nm]"),
                      yaxis=list(title="Temperature [\u00b0C]"),
                      zaxis=list(title="Intensity [a.u.]"))

  TL.3D <- layout(p = TL.3D,
                  title=TL.3D.title,
                  scene=TL.3D.scene)

  print(TL.3D)

  # OSL
  OSL.3D <- plot_ly(x=OSL.x,
                   y=OSL.y,
                   z=OSL.z,
                   type = "surface")

  OSL.3D.title <- paste("Intensity of the OSL emission of", name, "[u.a]")

  OSL.3D.scene <- list(xaxis=list(title="Emission wavelength [nm]"),
                      yaxis=list(title="Stimulation wavelength [nm]"),
                      zaxis=list(title="Intensity [a.u.]"))

  OSL.3D <- layout(p = OSL.3D,
                   title=OSL.3D.title,
                   scene=OSL.3D.scene)

  print(OSL.3D)
  #####################################
  }
