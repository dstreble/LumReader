#' Function to import a Stimulation
#'
#' This function import the properties of a Stimulation previously saved in a .Stimulation file.
#' @param file.name
#'  \link{character} name of the .FLT file containing the Stimulation properties.
#'
#' @return
#'  The function creates a new \code{\linkS4class{Stimulation}} object.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export import_Stimulation

import_Stimulation <- function(

  file.name

){
  if (missing(file.name)){
    stop("[import_Stimulation] Error: Input 'file.name' is missing.")
  }else if (!is(file.name,"character")){
    stop("[import_Stimulation] Error: Input 'file.name' is not of type 'character'.")
  }

  data <- readLines(file.name)

  Stimulation.name <- data[1]                                  ## 1st line contains "name: [name]"
  name <- strsplit(x = Stimulation.name,split = ":")[[1]][2]
  name <- gsub(pattern = " ",replacement = "", x = name)
  new.name <- as.character(name)

  Stimulation.description <- data[2]                           ## 2nd line contains "description: [description]"
  description <- strsplit(x = Stimulation.description,split = ": ")[[1]][2]
  new.description <- as.character(description)

  temp.emission <- data[3]                    ## 3th line contains "Quantum emission (s) [nm ; %]:"

  ## 4th-end line contain "[wavelength] ; [s]"
  Stimulation.emission <- data[4:length(data)]

  new.emission <- matrix(nrow = length(Stimulation.emission),
                           ncol = 2)

  for(i in 1: length(Stimulation.emission)){
    temp.emission <- Stimulation.emission[i]
    temp.emission <- unlist(strsplit(x=temp.emission, split = c(" ", ";", "\t")))
    temp.emission <- gsub(pattern = ",",replacement = ".", x = temp.emission)
    temp.emission <- suppressWarnings(as.numeric(temp.emission))
    temp.emission <- temp.emission[!is.na(temp.emission)]
    new.emission[i,] <- temp.emission
  }

  new.object <- setStimulation(name = new.name,
                       description = new.description,
                       emission = new.emission)

  return(new.object)
}
