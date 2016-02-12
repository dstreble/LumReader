#' Function to export a Stimulation
#'
#' This function exports a Stimulation as a .STI file.
#'
#' @param object
#'  \code{\linkS4class{Stimulation}} to export
#' @param file.name
#'  \link{character} name of the .FLT file that will contain the Stimulation properties.
#'
#' @return
#'  The function creates a new txt file containing the Stimulation properties.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export export_Stimulation

export_Stimulation <- function(

  object,

  file.name

){
  if (missing(object)){
    stop("[export_Stimulation] Error: Input 'object' is missing.")
  }else if (!is(object,"Stimulation")){
    stop("[export_Stimulation] Error: Input 'object' is not of type 'Stimulation'.")
  }

  if (missing(file.name)){
    stop("[export_Stimulation] Error: Input 'file.name' is missing.")
  }else if (!is(file.name,"character")){
    stop("[export_Stimulation] Error: Input 'file.name' is not of type 'character'.")
  }

  name <- object@name
  description <- object@description
  emission <- object@emission

  text <- vector()
  temp.text <- paste("Name:", name, "\n")
  cat(temp.text, file = file.name)
  temp.text <- paste("Description:", description, "\n")
  cat(temp.text,file = file.name,append = TRUE)
  temp.text <- paste("emission (s) [nm ; u.a]:", "\n")
  cat(temp.text, file = file.name,append = TRUE)

  for (i in 1: nrow(emission)){
    temp.text <- paste(text,emission[i,1], ";" ,emission[i,2], "\n")
    cat(temp.text, file = file.name,append = TRUE)
  }

}