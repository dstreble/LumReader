#' Function to plot a PMT
#'
#' This function plots the selected PMT.
#'
#' @param object
#'  \code{\linkS4class{PMT}} to plot
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @examples
#' PMT <- default_PMT('example')
#'
#' plot_PMT(PMT)
#'
#' @export plot_PMT

plot_PMT <- function(
  object

){
  if (missing(object)){
    stop("[plot_PMT] Error: Input 'object' is missing.")
  }else if (!is(object,"PMT")){
    stop("[plot_PMT] Error: Input 'object' is not of type 'PMT'.")
  }


  name <- object@name
  description <- object@description
  efficiency <- object@efficiency

  l <- efficiency[,1]
  s <- efficiency[,2]

  max.s <- max(s,na.rm = TRUE)

  plot.x.min <- 200
  plot.x.max <- 1000

  plot.y.min <- 0
  plot.y.max <- max.s*100

  plot(x = l,
       y = s*100,
       type = "l",
       xlim = c(plot.x.min,plot.x.max),
       ylim = c(plot.y.min,plot.y.max),
       main = name,
       sub = description,
       xlab = "Wavelength [nm]",
       ylab = "efficiency [%]")

}
