#' Fonction to upload the default Stimulation.
#'
#' This fonction generates a list containing the Stimulation included in the packages
#'
#' @param name
#'  \link{character} (with default): name of the Stimulation to import.
#'
#' @return
#' This function return a list containing the Stimulation included in the package.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export default_Stimulation

default_Stimulation <- function(
  name

){
  all.file.names <- dir(system.file("extdata", package="LumReader"))

  all.Stimulation.names <- vector()
  for(i in 1:length(all.file.names)){
    if(grepl(".EXI",all.file.names[i])){
      all.Stimulation.names <- c(all.Stimulation.names,all.file.names[i])
    }
  }
  all.Stimulation.names <- gsub(pattern = ".EXI",replacement = "", x = all.Stimulation.names)



  if (missing(name)){
    stop(paste("[default_Stimulation] Error: Input 'name' is missing. Available Stimulation are:", all.Stimulation.names))

  }else if (!is.character(name)){
    stop("[default_Stimulation] Error: Input 'name' is not of type 'characters'.")
  }

  if(name %in% all.Stimulation.names){
    file <- paste(name,".EXI",sep="")

  }else{
    stop(paste("[default_Stimulation] Warning: The Stimulation", name, "is not include in the package. Available Stimulation are:", all.Stimulation.names))
  }

  temp.path <- system.file("extdata", file, package="LumReader")

  new.Stimulation <- import_Stimulation(temp.path)


  return(new.Stimulation)
}
