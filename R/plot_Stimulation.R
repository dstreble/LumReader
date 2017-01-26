#' Function to plot a Stimulation
#'
#' This function plots the selected Stimulation.
#'
#' @param object
#'  \code{\linkS4class{Stimulation}} to plot
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @examples
#' stimulation <- default_Stimulation('example')
#'
#' plot_Stimulation(stimulation)
#'
#'
#' @export plot_Stimulation

plot_Stimulation <- function(
  object

){
  if (missing(object)){
    stop("[plot_Stimulation] Error: Input 'object' is missing.")
  }else if (!is(object,"Stimulation")){
    stop("[plot_Stimulation] Error: Input 'object' is not of type 'Stimulation'.")
  }

  name <- object@name
  description <- object@description
  emission <- object@emission

  l <- emission[,1]
  s <- emission[,2]

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
       ylab = "emission [u.a]")

}
