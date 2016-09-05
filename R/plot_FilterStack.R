#' Function to plot a filter stack.
#'
#' This function plots a filter stack.
#'
#' @param object
#'  \code{\linkS4class{FilterStack}} to plot.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export plot_FilterStack

plot_FilterStack <- function(

  object

){
  if (missing(object)){
    stop("[plot_FilterStack] Error: Input 'object' is missing.")
  }else if (!is(object,"FilterStack")){
    stop("[plot_FilterStack] Error: Input 'object' is not of type 'FilterStack'.")
  }


  title <- object@name
  subtitle <- object@description
  filters <- object@filters
  bunch <- object@bunch

  colors <- rainbow(length(filters)+1)

  legend.text <- vector()
  legend.col <- colors
  legend.pch <- vector()

  for (i in 1:length(filters)){

    temp.color <- colors[i+1]

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

    if(i ==1){
      plot(x = temp.x,
           y = temp.y,
           xlim = c(100,1200),
           ylim = c(0,100),
           main = title,
           sub= subtitle,
           xlab = "Wavelength [nm]",
           ylab = "Transmission [%]",
           type = "l",
           col= temp.color)

      par(new = TRUE)

    }else{
      lines(x = temp.x,
            y = temp.y,
            type = "l",
            col= temp.color)
    }
    legend.text <- c(legend.text,temp.name)
    legend.pch <- c(legend.pch, 18)
  }
  par(new = FALSE)

  if(!is.null(bunch)){
    par(new = TRUE)

    temp.filter <- bunch
    temp.name <- temp.filter@name
    temp.description <- temp.filter@description

    temp.color <- colors[1]

    d <- temp.filter@thickness
    rd <- temp.filter@reference.thickness
    r <- temp.filter@reflexion
    l <- temp.filter@transmission[,1]
    t <- temp.filter@transmission[,2]

    temp.x <- l
    temp.y <- r*t*100

    polygon(x = c(100,temp.x,1200),
            y = c(0,temp.y,0),
            col = temp.color)

    legend.text <- c(temp.name,legend.text)
    legend.pch <- c(18,legend.pch)

  }

  legend(x = "topleft",
         legend = legend.text,
         pch = legend.pch,
         col = legend.col,
         bty = "n")

  par(new = FALSE)
}
