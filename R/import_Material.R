#' Function to import a Material
#'
#' This function import the properties of a Material previously saved in a .Material file.
#' @param file.name
#'  \link{character} name of the .FLT file containing the Material properties.
#'
#' @return
#'  The function creates a new \code{\linkS4class{Material}} object.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export import_Material

import_Material <- function(

  file.name

){
  if (missing(file.name)){
    stop("[import_Material] Error: Input 'file.name' is missing.")
  }else if (!is(file.name,"character")){
    stop("[import_Material] Error: Input 'file.name' is not of type 'character'.")
  }

  new.file.name <- strsplit(x = file.name,split = "[.]")[[1]][1]

  # TL data

  TL.file.name <- paste(new.file.name, ".TL",sep = "")

  if(file.exists(TL.file.name)){
    TL.data <-  readLines(TL.file.name)

    TL.name <- TL.data[1]                                  ## 1st line contains "name: [name]"
    name <- strsplit(x = TL.name,split = ":")[[1]][2]
    name <- gsub(pattern = " ",replacement = "", x = name)
    new.TL.name <- as.character(name)

    TL.description <- TL.data[2]                           ## 2nd line contains "description: [description]"
    TL.description <- strsplit(x = TL.description,split = ": ")[[1]][2]
    new.TL.description <- as.character(TL.description)

    temp.TL <- TL.data[3]                    ## 3th line contains "TL [nm ; nm ;  a.u ]:"

    ## 4th-end line contain "[wavelength] ; [Temperature] ; [a.u.]"
    Material.TL <- TL.data[4:length(TL.data)]

    new.TL <- matrix(nrow = length(Material.TL),
                     ncol = 3)

    for(i in 1: length(Material.TL)){
      temp.TL <- Material.TL[i]
      temp.TL <- unlist(strsplit(x=temp.TL, split = " "))
      temp.TL <- unlist(strsplit(x=temp.TL, split = ";"))
      temp.TL <- unlist(strsplit(x=temp.TL, split = "\t"))
      temp.TL <- gsub(pattern = ",",replacement = ".", x = temp.TL)
      temp.TL <- suppressWarnings(as.numeric(temp.TL))
      temp.TL <- temp.TL[!is.na(temp.TL)]
      new.TL[i,] <- temp.TL
    }
  }else{
    new.TL.name <- NULL
    new.TL.description <- ""
    new.TL <- matrix(data=c(rep(seq(100,1200,10), each=50),
                            rep(seq(10,500,10),111),
                            rep(1,5550)),
                     nrow = 5550,
                     ncol = 3,
                     byrow = FALSE)
  }


  # OSL data

  OSL.file.name <- paste(new.file.name, ".OSL",sep = "")

  if(file.exists(OSL.file.name)){

    OSL.data <-  readLines(OSL.file.name)

    OSL.name <- OSL.data[1]                                  ## 1st line contains "name: [name]"
    name <- strsplit(x = OSL.name,split = ":")[[1]][2]
    name <- gsub(pattern = " ",replacement = "", x = name)
    new.OSL.name <- as.character(name)

    OSL.description <- OSL.data[2]                           ## 2nd line contains "description: [description]"
    OSL.description <- strsplit(x = OSL.description,split = ": ")[[1]][2]
    new.OSL.description <- as.character(OSL.description)

    temp.OSL <- OSL.data[3]                    ## 3th line contains "OSL [nm ; \u00b0C ;  a.u ]:"

    ## 4th-end line contain "[wavelength] ; [Temperature] ; [a.u.]"
    Material.OSL <- OSL.data[4:length(OSL.data)]

    new.OSL <- matrix(nrow = length(Material.OSL),
                     ncol = 3)

    for(i in 1: length(Material.OSL)){
      temp.OSL <- Material.OSL[i]
      temp.OSL <- unlist(strsplit(x=temp.OSL, split = " "))
      temp.OSL <- unlist(strsplit(x=temp.OSL, split = ";"))
      temp.OSL <- unlist(strsplit(x=temp.OSL, split = "\t"))
      temp.OSL <- gsub(pattern = ",",replacement = ".", x = temp.OSL)
      temp.OSL <- suppressWarnings(as.numeric(temp.OSL))
      temp.OSL <- temp.OSL[!is.na(temp.OSL)]
      new.OSL[i,] <- temp.OSL
    }
  }else{
    new.OSL.name <- NULL
    new.OSL.description <- ""
    new.OSL <- matrix(data=c(rep(seq(100,1200,10), each=81),
                             rep(seq(200,1000,10), 111),
                             rep(1,8991)),
                      nrow = 8991,
                      ncol = 3,
                      byrow = FALSE)
  }

  if(is.null(new.TL.name) && is.null(new.OSL.name)){
    stop("[import_Material] Error: No file containing the material TL or OSL properties.")
  }else if(is.null(new.TL.name)){
    new.name <- new.OSL.name
  }else if(is.null(new.OSL.name)){
    new.name <- new.TL.name
  }else if(new.TL.name != new.OSL.name){
    stop("[import_Material] Error: Input 'new.OSL.name' is different from 'new.TL.name'.")
  }else{
    new.name <- new.TL.name
  }

  new.Material <- setMaterial(name=new.name,
                              description.TL = new.TL.description,
                              TL = new.TL,
                              description.OSL = new.OSL.description,
                              OSL = new.OSL)


  return(new.Material)
}
