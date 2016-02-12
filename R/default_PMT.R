#' Fonction to upload the default PMT.
#'
#' This fonction generates a list containing the PMT included in the packages
#'
#' @param names
#'  \link{character} (with default): Names of the PMT to import.
#'
#' @return
#' This function return a list containing the PMT included in the package.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export default_PMT

default_PMT <- function(
  names

){
  all.file.names <- dir(system.file("extdata", package="LumFilters2"))

  all.PMT.names <- vector()
  for(i in 1:length(all.file.names)){
    if(grepl(".PMT",all.file.names[i])){
      all.PMT.names <- c(all.PMT.names,all.file.names[i])
    }
  }

  if (missing(names)){
    names <- all.PMT.names

  }else if (!is.character(names)){
    stop("[default_PMT] Error: Input 'names' is not of type 'characters'.")

  }else{
    names <- paste(names,".PMT",sep="")
  }

  PMT.names <- vector()

  for(i in 1: length(names)){
    if(names[i] %in% all.PMT.names){
      PMT.names <- c(PMT.names, names[i])
    }else{
      warning(paste("[default_PMT] Warning: The PMT", names[i], "is not include in the package."))
    }
  }


  list.PMT <- list()

  for(i in 1: length(PMT.names)){

    temp.path <- system.file("extdata", PMT.names[i],package="LumFilters2")

    temp.PMT <- import_PMT(temp.path)

    list.PMT <- c(list.PMT, temp.PMT)
  }
  if(length(list.PMT)>1){
    warning("[default_PMT] Warning: only the first PMT of the list is used")
  }

  new.PMT <- list.PMT[[1]]

  return(new.PMT)
}
