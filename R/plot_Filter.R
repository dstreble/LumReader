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
  r <- object@reflexion
  transmission <- object@transmission

  l <- transmission[,1]
  t <- transmission[,2]

  plot(x = l,
       y = r*t*100,
       type = "l",
       xlim = c(100,1200),
       ylim = c(0,100),
       main = name,
       sub = description,
       xlab = "Wavelength [nm]",
       ylab = "Transmission [%]")

}
