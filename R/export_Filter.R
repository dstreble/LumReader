#' Function to export a filter
#'
#' This function exports a filter as a .FLT file.
#'
#' @param object
#'  \code{\linkS4class{Filter}} to export
#' @param file.name
#'  \link{character} name of the .FLT file that will contain the filter properties.
#'
#' @return
#'  The function creates a new txt file containing the filter properties.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export export_Filter

export_Filter <- function(

  object,

  file.name

){
  if (missing(object)){
    stop("[export_Filter] Error: Input 'object' is missing.")
  }else if (!is(object,"Filter")){
    stop("[export_Filter] Error: Input 'object' is not of type 'Filter'.")
  }

  if (missing(file.name)){
    stop("[export_Filter] Error: Input 'file.name' is missing.")
  }else if (!is(file.name,"character")){
    stop("[export_Filter] Error: Input 'file.name' is not of type 'character'.")
  }

  name <- object@name
  description <- object@description
  thickness <- object@thickness
  reflexion <- object@reflexion
  transmission <- object@transmission

  text <- vector()
  temp.text <- paste("Name:", name, "\n")
  cat(temp.text, file = file.name)
  temp.text <- paste("Description:", description, "\n")
  cat(temp.text,file = file.name,append = TRUE)
  temp.text <- paste("Thickness [mm]:", thickness, "\n")
  cat(temp.text, file = file.name,append = TRUE)
  temp.text <- paste("Reflexion (1-P) [%]:", reflexion, "\n")
  cat(temp.text, file = file.name,append = TRUE)
  temp.text <- paste("Transmission (T) [nm ; %]:", "\n")
  cat(temp.text, file = file.name,append = TRUE)

  for (i in 1: nrow(transmission)){
    temp.text <- paste(text,transmission[i,1], ";" ,transmission[i,2], "\n")
    cat(temp.text, file = file.name,append = TRUE)
  }

}