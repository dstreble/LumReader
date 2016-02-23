#' Fonction to upload the default PMT.
#'
#' This fonction generates a list containing the PMT included in the packages
#'
#' @param name
#'  \link{character} (with default): name of the PMT to import.
#'
#' @return
#' This function return a list containing the PMT included in the package.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export default_PMT

default_PMT <- function(
  name

){
  all.file.names <- dir(system.file("extdata", package="LumReader"))

  all.PMT.names <- vector()
  for(i in 1:length(all.file.names)){
    if(grepl(".PMT",all.file.names[i])){
      all.PMT.names <- c(all.PMT.names,all.file.names[i])
    }
  }
  all.PMT.names <- gsub(pattern = ".PMT",replacement = "", x = all.PMT.names)



  if (missing(name)){
    stop(paste("[default_PMT] Error: Input 'name' is missing. Available PMT are:", all.PMT.names))

  }else if (!is.character(name)){
    stop("[default_PMT] Error: Input 'name' is not of type 'characters'.")
  }

  if(name %in% all.PMT.names){
      file <- paste(name,".PMT",sep="")
  }else{
    stop(paste("[default_PMT] Warning: The PMT", name, "is not include in the package. Available PMT are:", all.PMT.names))
  }

  temp.path <- system.file("extdata", file, package="LumReader")

  new.PMT <- import_PMT(temp.path)


  return(new.PMT)
}
