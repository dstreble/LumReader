#' Function to plot a Experiment
#'
#' This function plots the selected Experiment.
#'
#' @param object
#'  \code{\linkS4class{Experiment}} to plot
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @import lattice gridExtra
#'
#' @export plot_Experiment

plot_Experiment <- function(
  object

){
  if (missing(object)){
    stop("[plot_Experiment] Error: Input 'object' is missing.")
  }else if (!is(object,"Experiment")){
    stop("[plot_Experiment] Error: Input 'object' is not of type 'Experiment'.")
  }


  name <- object@name
  description <- object@description

  reader <- object@reader
  material <- object@material
  emission <- object@emission

  type <- object@type
  interval <- object@interval

  # Plot
  old.par <- par( no.readonly = TRUE )

  # Page 1: reader
  colors <- c("gray", "black", "red", "green","blue", "orange", "cyan", "brown", "chartreuse","chocolate", "coral", "cadetblue", "magenta", "blueviolet", rainbow(length(filters)))


  title <- reader@name
  subtitle <- reader@description
  stimulation <- reader@stimulation
  PMT <- reader@PMT
  filterStack <- reader@filterStack
  detection <- reader@detection

  colors <- c("gray", "black", "red", "green","blue", "orange", "cyan", "brown", "chartreuse","chocolate", "coral", "cadetblue", "magenta", "blueviolet", rainbow(length(filters)))

  if(!is.null(filterStack)){
    filters <- filterStack@filters
    filter.result <- filterStack@result

    colors <- c("gray", "black", "red", "green","blue", "orange", "cyan", "brown", "chartreuse","chocolate", "coral", "cadetblue", "magenta", "blueviolet", rainbow(length(filters)))

    if(!is.null(PMT) && !is.null(stimulation)){
      colors <- colors[1:(length(filters)+4)]

    }else if(!is.null(PMT)){
      colors <- colors[1:(length(filters)+3)]

    }else if(!is.null(stimulation)){
      colors <- colors[1:(length(filters)+2)]

    }else{
      colors <- colors[1:(length(filters)+1)]
    }

  }else{
    colors <- c("gray", "black", "red")
  }



  legend.text <- vector()
  legend.col <- colors
  legend.pch <- vector()

  # Plot
  old.par <- par( no.readonly = TRUE )
  par( oma = c(0.5, 0, 3, 0 ) )

  #Filter stack
  if(!is.null(filterStack)){

    #Filters results
    temp.filter <- filter.result
    temp.name <- temp.filter@name

    temp.color <- colors[4]

    r <- temp.filter@reflexion
    l <- temp.filter@transmission[,1]
    t <- temp.filter@transmission[,2]

    par(mar = c(5,5,4,5) )

    plot(x = l,
         y = r*t*100,
         xlim = c(100,1200),
         ylim = c(0,100),
         yaxt = "n",
         xaxt = "n",
         xlab = "",
         ylab = "",
         type="l",
         lwd=2,
         col= temp.color)

    axis(4)
    mtext(side = 4,
          text = "Transmission [%]",
          line = 2.5,
          cex = 0.8
    )

    par(new = TRUE)

    legend.text <- c(temp.name,legend.text)
    legend.pch <- c(18,legend.pch)


    # Filters
    for (i in 1:length(filters)){

      temp.filter <- filters[[i]]
      temp.name <- temp.filter@name
      temp.description <- temp.filter@description

      r <- temp.filter@reflexion
      l <- temp.filter@transmission[,1]
      t <- temp.filter@transmission[,2]

      temp.color <- colors[i+4]

      lines(x = l,
            y = r*t*100,
            lty=2,
            col= temp.color)

      par(new = TRUE)

      legend.text <- c(legend.text,temp.name)
      legend.pch <- c(legend.pch, 18)

    }
  }

  #Stimulation
  if(!is.null(stimulation)){
    temp.name <- stimulation@name
    temp.color <- colors[3]

    temp.l <- stimulation@emission[,1]
    temp.e <- stimulation@emission[,2]*100

    polygon(x = c(100,temp.l,1200),
            y = c(0,temp.e,0),
            col = temp.color,
            density=20)
    par(new = TRUE)

    legend.text <- c(temp.name,legend.text)
    legend.pch <- c(18, legend.pch)

    par(new = TRUE)
  }

  # PMT
  if(!is.null(PMT)){
    temp.name <- PMT@name
    temp.color <- colors[2]
    temp.l <- PMT@efficiency[,1]
    temp.s <- PMT@efficiency[,2]
    max.s<-max(temp.s)

    plot(x=temp.l,
         y=temp.s*100,
         xlim = c(100,1200),
         ylim = c(0,max.s*100),
         main = title,
         sub = subtitle,
         xlab =  "Wavelength [nm]",
         ylab = "Quantum efficiency [%]",
         type = "l",
         col=temp.color)

    legend.text <- c(temp.name,legend.text)
    legend.pch <- c(18, legend.pch)

    par(new = TRUE)
  }

  # Final detection
  if(!is.null(PMT) && !is.null(filterStack)){
    temp.name <- detection@name
    temp.color <- colors[1]

    temp.l <- detection@efficiency[,1]
    temp.s <- detection@efficiency[,2]*100

    polygon(x = c(100,temp.l,1200),
            y = c(0,temp.s,0),
            col = temp.color)
    par(new = TRUE)

    legend.text <- c(temp.name,legend.text)
    legend.pch <- c(18, legend.pch)

    par(new = TRUE)
  }

  legend(x = "topleft",
         legend = legend.text,
         pch = legend.pch,
         col = legend.col,
         bty = "n")

  par(new = FALSE)

  # page 2: material

    # Layout
  par( oma = c(0.5, 0, 3, 0 ) )

  if(type == "TL"){

    material.name <- material@name
    material.description <- material@description.TL
    material.signal <- material@TL

    TL.wavelength <- material.signal[,1]
    TL.temperature <- material.signal[,2]
    TL.signal <- material.signal[,3]

    material.x <- unique(TL.wavelength)
    material.y <- unique(TL.temperature)
    material.z <- matrix(data=TL.signal,
                   nrow = length(material.x),
                   ncol = length(material.y),
                   byrow = TRUE)

    material.levelplot <- levelplot(x= material.z,
                                    row.values=material.x,
                                    column.values=material.y,
                                    xlab="Emission wavelength [nm]",
                                    ylab="Temperature [\u00b0C]",
                                    main=paste("Intensity of the",type, "emission of", name, "[u.a]"),
                                    cuts=39,
                                    col.regions=rev(heat.colors(n = 40,alpha = 1)),
                                    colorkey=TRUE,
                                    panel = function(...){
                                      panel.levelplot(...)
                                      panel.abline(h = interval[1])
                                      panel.abline(h = interval[2])
                                    })

  }else if(type == "OSL"){
    material.name <- material@name
    material.description <- material@description.OSL
    material.signal <- material@OSL

    OSL.wavelength <- material.signal[,1]
    OSL.temperature <- material.signal[,2]
    OSL.signal <- material.signal[,3]

    material.x <- unique(OSL.wavelength)
    material.y <- unique(OSL.temperature)
    material.z <- matrix(data=OSL.signal,
                         nrow = length(material.x),
                         ncol = length(material.y),
                         byrow = TRUE)

    material.levelplot <- levelplot(x= material.z,
                                    row.values=material.x,
                                    column.values=material.y,
                                    xlab="Emission wavelength [nm]",
                                    ylab="Stimulation wavelength [nm]",
                                    main=paste("Intensity of the",type, "emission of", name, "[u.a]"),
                                    cuts=39,
                                    col.regions=rev(terrain.colors(n = 40,alpha = 1)),
                                    colorkey=TRUE,
                                    panel = function(...){
                                      panel.levelplot(...)
                                      panel.abline(h = interval[1])
                                      panel.abline(h = interval[2])
                                    })
  }

  grid.arrange(material.levelplot)


  #page 3: emission
    #Layout
  par( oma = c(0.5, 0, 3, 0 ) )

  colors <- c("orange", "blue", "black","red")

  title <- name
  subtitle <- description

  legend.text <- vector()
  legend.col <- vector()
  legend.pch <- vector()

    # Stimulation
  par(mar = c(5,5,4,5) )

  temp.name <- reader@stimulation@description
  temp.color <- colors[1]
  temp.x <- reader@stimulation@emission[,1]
  temp.y <- reader@stimulation@emission[,2]

  plot(x = temp.x,
       y = temp.y,
       xlim = c(100,1200),
       ylim = c(0,max(temp.y)),
       yaxt = "n",
       xaxt = "n",
       xlab = "",
       ylab = "",
       type="l",
       col= temp.color)

  axis(4)
  mtext(side = 4,
        text = "Signal intensity [a.u.]",
        line = 2.5,
        cex = 0.8
  )

  polygon(x = c(100,temp.x,1200),
          y = c(0,temp.y,0),
          col = temp.color,
          density=20)

  par(new = TRUE)

  legend.text <- c(legend.text,temp.name)
  legend.pch <- c(legend.pch, 18)
  legend.col <- c(legend.col,temp.color)


  # Emission

  temp.name <- emission@description
  temp.color <- colors[2]
  temp.x <- emission@emission[,1]
  temp.y <- emission@emission[,2]

  polygon(x = c(100,temp.x,1200),
          y = c(0,temp.y,0),
          col = temp.color,
          density=20)
  par(new = TRUE)

  legend.text <- c(legend.text,temp.name)
  legend.pch <- c(legend.pch, 18)
  legend.col <- c(legend.col,temp.color)


  # Detection windows

  temp.name <- detection@description
  temp.color <- colors[3]
  temp.x <- detection@efficiency[,1]
  temp.y <- detection@efficiency[,2]*100

  plot(x=temp.x,
       y=temp.y,
       xlim = c(100,1200),
       ylim = c(0,max(temp.y)),
       main = title,
       sub = subtitle,
       xlab =  "Wavelength [nm]",
       ylab = "Reader quantum efficiency [%]",
       type = "l",
       lwd=2,
       col=temp.color)

  par(new = TRUE)

  legend.text <- c(temp.name,legend.text)
  legend.pch <- c(18, legend.pch)
  legend.col <- c(temp.color,legend.col)

  # Interval

  if(type=="OSL"){
    temp.name <- "Stimulation interval"
    temp.color <- colors[4]

    abline(v=interval[1],
           lty=2,
           col=temp.color)
    abline(v=interval[2],
           lty=2,
           col=temp.color)

    legend.text <- c(legend.text,temp.name)
    legend.pch <- c(legend.pch, 18)
    legend.col <- c(legend.col,temp.color)
  }
  #Legend

  legend(x = "topleft",
         legend = legend.text,
         pch = legend.pch,
         col = legend.col,
         bty = "n")

  par(new = FALSE)

  par(old.par)
}
