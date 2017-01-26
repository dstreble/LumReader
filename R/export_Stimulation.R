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
#' @examples
#' example <- default_Stimulation('example')
#' file.name <- 'example'
#'
#' \dontrun{
#' export_Stimulation(example, file.name) # uncomment
#' }
#'
#' # There is now an 'example.EXI' file in the 'working directory'.
#' # This file is a classical .txt file despite the extension.
#'
#' file <- paste(getwd(),'/', file.name,'.EXI', sep="")
#' print(file)
#' # readLines(file)
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

  new.file <- strsplit(x = file.name,split = "[.]")[[1]][1]
  new.file <- paste(new.file,".EXI",sep = "")

  name <- object@name
  description <- object@description
  type <- object@type
  emission <- object@emission

  text <- vector()
  temp.text <- paste("Name:", name, "\n")
  cat(temp.text, file =   new.file)
  temp.text <- paste("Description:", description, "\n")
  cat(temp.text,file =    new.file,append = TRUE)
  temp.text <- paste("Type:", type, "\n")
  cat(temp.text,file =    new.file,append = TRUE)
  temp.text <- paste("emission (s) [nm ; u.a]:", "\n")
  cat(temp.text, file = new.file,append = TRUE)

  for (i in 1: nrow(emission)){
    temp.text <- paste(text,emission[i,1], ";" ,emission[i,2], "\n")
    cat(temp.text, file =   new.file,append = TRUE)
  }

}
