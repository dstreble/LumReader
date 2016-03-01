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


  name <- object@name
  description <- object@description
  d <- object@thickness
  rd <- object@reference.thickness
  r <- object@reflexion
  transmission <- object@transmission

  t <-transmission[,2]

  temp.x <- transmission[,1]
  temp.y <- r*(t^(d/rd))*100

  plot(x = temp.x,
       y = temp.y,
       type = "l",
       xlim = c(100,1200),
       ylim = c(0,100),
       main = name,
       sub = description,
       xlab = "Wavelength [nm]",
       ylab = "Transmission [%]")

}
