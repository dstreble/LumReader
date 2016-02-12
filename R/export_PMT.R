#' Function to export a PMT
#'
#' This function exports a PMT as a .PMT file.
#'
#' @param object
#'  \code{\linkS4class{PMT}} to export
#' @param file.name
#'  \link{character} name of the .FLT file that will contain the PMT properties.
#'
#' @return
#'  The function creates a new txt file containing the PMT properties.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export export_PMT

export_PMT <- function(

  object,

  file.name

){
  if (missing(object)){
    stop("[export_PMT] Error: Input 'object' is missing.")
  }else if (!is(object,"PMT")){
    stop("[export_PMT] Error: Input 'object' is not of type 'PMT'.")
  }

  if (missing(file.name)){
    stop("[export_PMT] Error: Input 'file.name' is missing.")
  }else if (!is(file.name,"character")){
    stop("[export_PMT] Error: Input 'file.name' is not of type 'character'.")
  }

  name <- object@name
  description <- object@description
  efficiency <- object@efficiency

  text <- vector()
  temp.text <- paste("Name:", name, "\n")
  cat(temp.text, file = file.name)
  temp.text <- paste("Description:", description, "\n")
  cat(temp.text,file = file.name,append = TRUE)
  temp.text <- paste("Quantum efficiency (QE) [nm ; %]:", "\n")
  cat(temp.text, file = file.name,append = TRUE)

  for (i in 1: nrow(efficiency)){
    temp.text <- paste(text,efficiency[i,1], ";" ,efficiency[i,2], "\n")
    cat(temp.text, file = file.name,append = TRUE)
  }

}
