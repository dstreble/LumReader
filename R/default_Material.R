#' Fonction to upload the default Material.
#'
#' This fonction generates a list containing the Material included in the packages
#'
#' @param name
#'  \link{character} (with default): name of the Material to import.
#'
#' @return
#' This function return a list containing the Material included in the package.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export default_Material

default_Material <- function(
  name

){
  all.file.names <- dir(system.file("extdata", package="LumReader"))

  all.Material.names <- vector()
  for(i in 1:length(all.file.names)){
    if(grepl(".TL",all.file.names[i])){
      all.Material.names <- c(all.Material.names,all.file.names[i])
    }else if(grepl(".OSL",all.file.names[i])){
      all.Material.names <- c(all.Material.names,all.file.names[i])
    }
  }
  all.Material.names <- gsub(pattern = ".TL",replacement = "", x = all.Material.names)
  all.Material.names <- gsub(pattern = ".OSL",replacement = "", x = all.Material.names)



  if (missing(name)){
    stop(paste("[default_Material] Error: Input 'name' is missing. Available Material are:", all.Material.names))

  }else if (!is.character(name)){
    stop("[default_Material] Error: Input 'name' is not of type 'characters'.")
  }

  if(name %in% all.Material.names){
    file <- name
  }else{
    stop(paste("[default_Material] Warning: The Material", name, "is not include in the package. Available Material are:", all.Material.names))
  }

  temp.path <- system.file("extdata", file, package="LumReader")

  new.Material <- import_Material(temp.path)


  return(new.Material)
}
