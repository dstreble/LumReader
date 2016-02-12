#' Function to import a filter
#'
#' This function import the properties of a filter previously saved in a .FLT file.
#' @param file.name
#'  \link{character} name of the .FLT file containing the filter properties.
#'
#' @return
#'  The function creates a new \code{\linkS4class{Filter}} object.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export import_Filter

import_Filter <- function(

  file.name

){
  if (missing(file.name)){
    stop("[import_Filter] Error: Input 'file.name' is missing.")
  }else if (!is(file.name,"character")){
    stop("[import_Filter] Error: Input 'file.name' is not of type 'character'.")
  }

  data <- readLines(file.name)

  filter.name <- data[1]                                  ## 1st line contains "name: [name]"
  name <- strsplit(x = filter.name,split = ":")[[1]][2]
  name <- gsub(pattern = " ",replacement = "", x = name)
  new.name <- as.character(name)

  filter.description <- data[2]                           ## 2nd line contains "description: [description]"
  description <- strsplit(x = filter.description,split = ": ")[[1]][2]
  new.description <- as.character(description)

  filter.thickness <- data[3]                           ## 3rd line contains "thickness [mm]: [thickness]"
  thickness <- strsplit(x = filter.thickness,split = ": ")[[1]][2]
  thickness <- gsub(pattern = ",",replacement = ".", x = thickness)
  new.thickness <- as.numeric(thickness)

  filter.reflexion <- data[4]                          ## 4th line contains "reflexion (1-P) [%]: [reflexion]"
  reflexion <- strsplit(x = filter.reflexion,split = ": ")[[1]][2]
  reflexion <- gsub(pattern = ",",replacement = ".", x = reflexion)
  new.reflexion <- as.numeric(reflexion)

  temp.transmission <- data[5]                    ## 5th line contains "Transmission (T) [%]:"
  ## 6th-end line contain "[wavelength] [tau]"
  filter.transmission <- data[6:length(data)]

  new.transmission <- matrix(nrow = length(filter.transmission),
                             ncol = 2)

  for(i in 1: length(filter.transmission)){
    temp.transmission <- filter.transmission[i]
    temp.transmission <- unlist(strsplit(x=temp.transmission, split = c(" ", ";", "\t")))
    temp.transmission <- gsub(pattern = ",",replacement = ".", x = temp.transmission)
    temp.transmission <- suppressWarnings(as.numeric(temp.transmission))
    temp.transmission <- temp.transmission[!is.na(temp.transmission)]
    new.transmission[i,] <- temp.transmission
  }

  new.object <- setFilter(name = new.name,
                          description = new.description,
                          thickness = new.thickness,
                          reflexion = new.reflexion,
                          transmission = new.transmission)

  return(new.object)
}
