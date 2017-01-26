#' Function to import a Stimulation
#'
#' This function import the properties of a Stimulation previously saved in a .EXI file.
#' @param file.name
#'  \link{character} name of the .FLT file containing the Stimulation properties.
#'
#' @return
#'  The function creates a new \code{\linkS4class{Stimulation}} object.
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
#' example <- import_Stimulation(file)
#'
#' plot_Stimulation(example)
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

  new.file.name <- file_path_sans_ext(file.name)
  ext <- ".EXI"
  new.file.name <- paste(new.file.name,ext,sep = "")

  data <- readLines(new.file.name)

  Stimulation.name <- data[1]                                  ## 1st line contains "name: [name]"
  name <- strsplit(x = Stimulation.name,split = ":")[[1]][2]
  name <- gsub(pattern = " ",replacement = "", x = name)
  new.name <- as.character(name)

  Stimulation.description <- data[2]                           ## 2nd line contains "description: [description]"
  description <- strsplit(x = Stimulation.description,split = ": ")[[1]][2]
  new.description <- as.character(description)

  Stimulation.type <- data[3]                           ## 3nd line contains "type: [type]"
  type <- strsplit(x = Stimulation.type,split = ": ")[[1]][2]
  new.type <- as.character(type)

  if(grepl(pattern = "tl",x = tolower(new.type))){
    new.type <- "TL"
  }else if(grepl(pattern = "osl",x = tolower(new.type))){
    new.type <- "OSL"
  }else{
    stop(paste("[import_Stimulation] Error: the 'type'", new.type ,"of", file.name, "is not supported."))
  }

  temp.emission <- data[4]                                    ## 3th line contains "Quantum emission (s) [nm ; u.a]:"

  Stimulation.emission <- data[5:length(data)]                ## 4th-end line contain "[wavelength ; u.a]"


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
                       type=new.type,
                       emission = new.emission)

  return(new.object)
}
