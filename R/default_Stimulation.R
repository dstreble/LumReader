#' Fonction to upload the default stimulation.
#'
#' This fonction generates a list containing the stimulation included in the packages
#'
#' @param names
#'  \link{character} (with default): Names of the stimulation to import.
#'
#' @return
#' This function return a list containing the stimulation included in the package.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export default_Stimulation

default_Stimulation <- function(
  names

){
  all.file.names <- dir(system.file("extdata", package="LumReader"))

  all.stimulation.names <- vector()
  for(i in 1:length(all.file.names)){
    if(grepl(".STI",all.file.names[i])){
      all.stimulation.names <- c(all.stimulation.names,all.file.names[i])
    }
  }

  if (missing(names)){
    names <- all.stimulation.names

  }else if (!is.character(names)){
    stop("[default_Stimulation] Error: Input 'names' is not of type 'characters'.")

  }else{
    names <- paste(names,".STI",sep="")
  }

  stimulation.names <- vector()

  for(i in 1: length(names)){
    if(names[i] %in% all.stimulation.names){
      stimulation.names <- c(stimulation.names, names[i])
    }else{
      warning(paste("[default_Stimulation] Warning: The stimulation", names[i], "is not include in the package."))
    }
  }


  list.stimulation <- list()

  for(i in 1: length(stimulation.names)){

    temp.path <- system.file("extdata", stimulation.names[i],package="LumFilters2")

    temp.stimulation <- import_Stimulation(temp.path)

    list.stimulation <- c(list.stimulation, temp.stimulation)
  }

  if(length(list.stimulation)>1){
    warning("[default_Stimulation] Warning: only the first stimulation of the list is used")
  }

  new.stimulation <- list.stimulation[[1]]

  return(new.stimulation)
}
