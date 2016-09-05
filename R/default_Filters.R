#' Fonction to upload the default filters.
#'
#' This fonction generates a list containing the filters included in the packages
#'
#' @param names
#'  \link{character} (with default): Names of the filters to import.
#'
#' @param thickness
#'  \link{character} (with default): thickness of the filters to import.
#'
#' @return
#' This function return a list containing the filters included in the package.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export default_Filters

default_Filters <- function(
  names,

  thickness=NULL

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

  if(is.null(thickness)){
    thickness <- vector("numeric",length(names))

  }else if(length(thickness) != length(names)){
    stop("[default_Filters] Error: Input 'names' and 'thickness' do not have the same length.")
  }


  filter.names <- vector()
  filter.thickness <- vector()

  for(i in 1: length(names)){
    if(names[i] %in% all.filter.names){
      filter.names <- c(filter.names, names[i])
      filter.thickness <- c(filter.thickness, thickness[i])
    }else{
      warning(paste("[default_Filters] Warning: The filter", names[i], "is not include in the package."))
    }
  }

  list.filters <- list()

  if(length(filter.names) > 0){
    for(i in 1: length(filter.names)){

      temp.path <- system.file("extdata", filter.names[i],package="LumReader")

      if(filter.thickness[i] != 0){
        temp.filter <- import_Filter(temp.path, filter.thickness[i])
      }else{
        temp.filter <- import_Filter(temp.path)
      }

      list.filters <- c(list.filters, temp.filter)
    }
  }else{
    stop("[default_Filters] Error: No filters in the filter list.")
  }

  return(list.filters)
}
