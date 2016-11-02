#' Function to plot a filter
#'
#' This function plots the selected filter.
#'
#' @param object
#'  \code{\linkS4class{Filter}} to plot
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#'
#' @export plot_Filter

plot_Filter <- function(
  object

){
  if (missing(object)){
    stop("[plot_Filter] Error: Input 'object' is missing.")
  }else if (!is(object,"Filter")){
    stop("[plot_Filter] Error: Input 'object' is not of type 'Filter'.")
  }

  plot.x.min <- 200
  plot.x.max <- 1000

  plot.y.min <- 0
  plot.y.max <- 100

  name <- object@name
  description <- object@description
  r <- object@reflexion
  transmission <- object@transmission

  t <-transmission[,2]

  temp.x <- transmission[,1]
  temp.y <- r*t*100

  plot(x = temp.x,
       y = temp.y,
       type = "l",
       xlim = c(plot.x.min,plot.x.max),
       ylim = c(plot.y.min,plot.y.max),
       main = name,
       sub = description,
       xlab = "Wavelength [nm]",
       ylab = "Transmission [%]")

}
