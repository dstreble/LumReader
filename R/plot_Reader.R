#' Function to plot a filter stack.
#'
#' This function plots a filter stack.
#'
#' @param object
#'  \code{\linkS4class{Reader}} to plot.
#'
#' @author David Strebler, University of Cologne (Germany).
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

  colors <- c("gray", "black", "red", "green","blue", "orange", "cyan", "brown", "chartreuse","chocolate", "coral", "cadetblue", "magenta", "blueviolet", rainbow(length(filters)))


  title <- object@name
  subtitle <- object@description
  stimulation <- object@stimulation
  PMT <- object@PMT
  filterStack <- object@filterStack

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

  # Final result
  if(!is.null(PMT) && !is.null(filterStack)){
    temp.name <- "Resulting efficiency"
    temp.color <- colors[1]

    temp.r <- filter.result@reflexion
    temp.t <- filter.result@transmission[,2]

    temp.l <- PMT@efficiency[,1]
    temp.s <- PMT@efficiency[,2]*temp.r*temp.t*100

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
}
