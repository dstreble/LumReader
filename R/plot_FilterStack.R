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
  result <- object@result

  colors <- rainbow(length(filters)+1)

  legend.text <- vector()
  legend.col <- colors
  legend.pch <- vector()

  for (i in 1:length(filters)){
    temp.filter <- filters[[i]]
    temp.name <- temp.filter@name
    temp.description <- temp.filter@description

    d <- temp.filter@thickness
    r <- temp.filter@reflexion
    l <- temp.filter@transmission[,1]
    t <- temp.filter@transmission[,2]

    temp.color <- colors[i+1]

    if(i ==1){
      plot(x = l,
           y = r*t*100,
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
      lines(x = l,
            y = r*t*100,
            type = "l",
            col= temp.color)
    }
    legend.text <- c(legend.text,temp.name)
    legend.pch <- c(legend.pch, 18)
  }
  par(new = FALSE)

  if(!is.null(result)){
    par(new = TRUE)

    temp.filter <- result
    temp.name <- temp.filter@name
    temp.description <- temp.filter@description

    temp.color <- colors[1]

    d <- temp.filter@thickness
    r <- temp.filter@reflexion
    l <- temp.filter@transmission[,1]
    t <- temp.filter@transmission[,2]

    polygon(x = c(100,l,1200),
            y = c(0,r*t*100,0),
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
