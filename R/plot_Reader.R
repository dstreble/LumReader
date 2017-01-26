#' Function to plot a filter stack.
#'
#' This function plots a filter stack.
#'
#' @param object
#'  \code{\linkS4class{Reader}} to plot.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @examples
#' # Data
#' name <- 'example'
#' description <- 'example'
#'
#' # Create reader components
#' filter <- default_Filters('example')
#' filterStack <- create_FilterStack(name, description, filter)
#' stimulation <- default_Stimulation('example')
#' PMT <- default_PMT('example')
#'
# Create reader
#' reader <- create_Reader(name, description, stimulation, filterStack, PMT)
#'
#' plot_Reader(reader)
#'
#' @export plot_Reader

plot_Reader <- function(

  object

){
  if (missing(object)){
    stop("[plot_Reader] Error: Input 'object' is missing.")
  }else if (!is(object,"Reader")){
    stop("[plot_Reader] Error: Input 'object' is not of type 'Reader'.")
  }

  #colors <- c("gray", "black", "red", "green","blue", "orange", "cyan", "brown", "chartreuse","chocolate", "coral", "cadetblue", "magenta", "blueviolet", rainbow(length(filters)))


  title <- object@name
  subtitle <- object@description
  stimulation <- object@stimulation
  PMT <- object@PMT
  filterStack <- object@filterStack
  detection <- object@detection

  colors <- c("gray", "black", "red", "green","blue", "orange", "cyan", "brown", "chartreuse","chocolate", "coral", "cadetblue", "magenta", "blueviolet")

  if(!is.null(filterStack)){
    filters <- filterStack@filters
    filter.bunch <- filterStack@bunch

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

  plot.x.min <- 200
  plot.x.max <- 1000

  plot.y.min <- 0
  plot.y.max <- 100

  #Filter stack
  if(!is.null(filterStack)){

    #Filters bunchs
    temp.filter <- filter.bunch
    temp.name <- temp.filter@name

    temp.color <- colors[4]

    d <- temp.filter@thickness
    rd <- temp.filter@reference.thickness

    r <- temp.filter@reflexion
    l <- temp.filter@transmission[,1]
    t <- temp.filter@transmission[,2]

    temp.x <- l
    temp.y <- r*t*100

    par(mar = c(5,5,4,5) )

    plot(x = temp.x,
         y = temp.y,
         xlim = c(plot.x.min,plot.x.max),
         ylim = c(plot.y.min,plot.y.max),
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

      temp.color <- colors[i+4]

      temp.filter <- filters[[i]]
      temp.name <- temp.filter@name
      temp.description <- temp.filter@description


      d <- temp.filter@thickness
      rd <- temp.filter@reference.thickness
      r <- temp.filter@reflexion
      l <- temp.filter@transmission[,1]
      t <- temp.filter@transmission[,2]

      temp.x <- l
      temp.y <- r*t*100

      lines(x = temp.x,
            y = temp.y,
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

    polygon(x = c(plot.x.min,temp.l,plot.x.max),
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

    plot.y.max <- max.s*100

    plot(x=temp.l,
         y=temp.s*100,
         xlim = c(plot.x.min, plot.x.max),
         ylim = c(plot.y.min, plot.y.max),
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

    polygon(x = c(plot.x.min,temp.l,plot.x.max),
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
}
