#' Function to export a Material
#'
#' This function exports a Material as a .Material file.
#'
#' @param object
#'  \code{\linkS4class{Material}} to export
#' @param file.name
#'  \link{character} name of the .FLT file that will contain the Material properties.
#'
#' @return
#'  The function creates a new txt file containing the Material properties.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export export_Material

export_Material <- function(

  object,

  file.name

){
  if (missing(object)){
    stop("[export_Material] Error: Input 'object' is missing.")
  }else if (!is(object,"Material")){
    stop("[export_Material] Error: Input 'object' is not of type 'Material'.")
  }

  if (missing(file.name)){
    stop("[export_Material] Error: Input 'file.name' is missing.")
  }else if (!is(file.name,"character")){
    stop("[export_Material] Error: Input 'file.name' is not of type 'character'.")
  }

  new.file <- strsplit(x = file.name,split = "[.]")[[1]][1]

  # TL file

  new.TL.file <- paste(new.file,".TL",sep = "")

  name <- object@name
  description.TL <- object@description.TL

  TL <- object@TL

  text <- vector()
  temp.text <- paste("Name:", name, "\n")
  cat(temp.text, file = new.TL.file)
  temp.text <- paste("Description:", description.TL, "\n")
  cat(temp.text,file = new.TL.file,append = TRUE)
  temp.text <- paste("TL emission [nm ; \u00b0C ; a.u.]:", "\n")
  cat(temp.text, file = new.TL.file,append = TRUE)

  for (i in 1: nrow(TL)){
    temp.text <- paste(text,TL[i,1], ";" ,TL[i,2], " ; ", TL[i,3], "\n")
    cat(temp.text, file = new.TL.file,append = TRUE)
  }

  # OSL

  new.OSL.file <- paste(new.file,".OSL",sep = "")

  name <- object@name
  description.OSL <- object@description.OSL

  OSL <- object@OSL

  text <- vector()
  temp.text <- paste("Name:", name, "\n")
  cat(temp.text, file = new.OSL.file)
  temp.text <- paste("Description:", description.OSL, "\n")
  cat(temp.text,file = new.OSL.file,append = TRUE)
  temp.text <- paste("OSL emission [nm ; nm ; a.u.]:", "\n")
  cat(temp.text, file = new.OSL.file,append = TRUE)

  for (i in 1: nrow(OSL)){
    temp.text <- paste(text,OSL[i,1], ";" ,OSL[i,2], " ; ", OSL[i,3], "\n")
    cat(temp.text, file = new.OSL.file,append = TRUE)
  }
}
