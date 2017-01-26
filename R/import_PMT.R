#' Function to import a PMT
#'
#' This function import the properties of a PMT previously saved in a .PMT file.
#' @param file.name
#'  \link{character} name of the .FLT file containing the PMT properties.
#'
#' @return
#'  The function creates a new \code{\linkS4class{PMT}} object.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @examples
#' folder <- system.file("extdata", package="LumReader")
#'
#' file.name <- 'example' # !!! no extension !!! #
#'
#' file <-paste(folder, '/', file.name, sep="")
#'
#' example <- import_PMT(file)
#'
#' plot_PMT(example)
#'
#' @export import_PMT

import_PMT <- function(

  file.name

){
  if (missing(file.name)){
    stop("[import_PMT] Error: Input 'file.name' is missing.")
  }else if (!is(file.name,"character")){
    stop("[import_PMT] Error: Input 'file.name' is not of type 'character'.")
  }

  new.file.name <- file_path_sans_ext(file.name)
  ext <- ".PMT"
  new.file.name <- paste(new.file.name,ext,sep = "")

  data <- readLines(new.file.name)

  PMT.name <- data[1]                                  ## 1st line contains "name: [name]"
  name <- strsplit(x = PMT.name,split = ":")[[1]][2]
  name <- gsub(pattern = " ",replacement = "", x = name)
  new.name <- as.character(name)

  PMT.description <- data[2]                           ## 2nd line contains "description: [description]"
  description <- strsplit(x = PMT.description,split = ": ")[[1]][2]
  new.description <- as.character(description)

  temp.efficiency <- data[3]                    ## 3th line contains "Quantum efficiency (s) [nm ; %]:"

  ## 4th-end line contain "[wavelength] ; [s]"
  PMT.efficiency <- data[4:length(data)]

  new.efficiency <- matrix(nrow = length(PMT.efficiency),
                             ncol = 2)

  for(i in 1: length(PMT.efficiency)){
    temp.efficiency <- PMT.efficiency[i]
    temp.efficiency <- unlist(strsplit(x=temp.efficiency, split = " "))
    temp.efficiency <- unlist(strsplit(x=temp.efficiency, split = ";"))
    temp.efficiency <- unlist(strsplit(x=temp.efficiency, split = "\t"))
    temp.efficiency <- gsub(pattern = ",",replacement = ".", x = temp.efficiency)
    temp.efficiency <- suppressWarnings(as.numeric(temp.efficiency))
    temp.efficiency <- temp.efficiency[!is.na(temp.efficiency)]
    new.efficiency[i,] <- temp.efficiency
  }

  new.object <- setPMT(name = new.name,
                       description = new.description,
                       efficiency = new.efficiency)

  return(new.object)
}
