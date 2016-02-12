#' Fonction to upload the default filters.
#'
#' This fonction generates a list containing the filters included in the packages
#'
#' @param names
#'  \link{character} (with default): Names of the filters to import.
#'
#' @return
#' This function return a list containing the filters included in the package.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export default_Filters

default_Filters <- function(
  names

){
  all.file.names <- dir(system.file("extdata", package="LumReader"))

  all.filter.names <- vector()

  for(i in 1:length(all.file.names)){
    if(grepl(".FLT",all.file.names[i])){
      all.filter.names <- c(all.filter.names,all.file.names[i])
    }
  }

  if (missing(names)){
    names <- all.filter.names

  }else if (!is.character(names)){
    stop("[default_Filters] Error: Input 'names' is not of type 'characters'.")

  }else{
    names <- paste(names,".FLT",sep="")
  }

  filter.names <- vector()

  for(i in 1: length(names)){
    if(names[i] %in% all.filter.names){
      filter.names <- c(filter.names, names[i])
    }else{
      warning(paste("[default_Filters] Warning: The filter", names[i], "is not include in the package."))
    }
  }


  list.filters <- list()

  for(i in 1: length(filter.names)){

    temp.path <- system.file("extdata", filter.names[i],package="LumFilters2")

    temp.filter <- import_Filter(temp.path)

    list.filters <- c(list.filters, temp.filter)
  }

  return(list.filters)
}
