#' Function to create a Material.
#'
#' This function create a new Material.
#'
#' @param name
#'  \link{character}: Name of the material.
#'
#' @param description.TL
#'  \link{character}: description of the material TL properties.
#'
#' @param TL
#'  \link{numeric}: TL response of the Material [a.u.].
#'
#' @param description.OSL
#'  \link{character}: description of the material OSL properties.
#'
#' @param OSL
#'  \link{numeric}: TL of the Material [a.u.].
#'
#' @return
#'  This function return a new Material.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export create_Material

create_Material <- function(

  name,

  description.TL,

  description.OSL,

  TL,

  OSL
){

  if (missing(name)){
    stop("[create_Material] Error: Input 'name' is missing.")
  }else if (!is.character(name)){
    stop("[create_Material] Error: Input 'name' is not of type 'character'.")
  }

  if (missing(description.TL)){
    stop("[create_Material] Error: Input 'description.TL' is missing.")
  }else if (!is.character(description.TL)){
    stop("[create_Material] Error: Input 'description.TL' is not of type 'character'.")
  }

  if (missing(TL)){
    stop("[create_Material] Error: Input 'TL' is missing.")
  }else if(!is.numeric(TL[,1])){
    stop("[create_Material] Error: Input 'TL[,1]' is not of type 'numeric'.")
  }else if(min(TL[,1])<=0){
    stop("[create_Material] Error: Input value of 'TL[,1]' has to be > 0.")
  }else if(!is.numeric(TL[,2])){
    stop("[create_Material] Error: Input 'TL[,2]' is not of type 'numeric'.")
  }else if(min(TL[,2])<0){
    stop("[create_Material] Error: Input value of 'TL[,2]' has to be >= 0.")
  }else if(!is.numeric(TL[,3])){
    stop("[create_Material] Error: Input 'TL[,3]' is not of type 'numeric'.")
  }else if(min(TL[,3])<0){
    stop("[create_Material] Error: Input value of 'TL[,3]' has to be >= 0.")
  }

  if (missing(description.OSL)){
    stop("[create_Material] Error: Input 'description.OSL' is missing.")
  }else if (!is.character(description.OSL)){
    stop("[create_Material] Error: Input 'description.OSL' is not of type 'character'.")
  }

  if (missing(OSL)){
    stop("[create_Material] Error: Input 'OSL' is missing.")
  }else if(!is.numeric(OSL[,1])){
    stop("[create_Material] Error: Input 'OSL[,1]' is not of type 'numeric'.")
  }else if(min(OSL[,1])<=0){
    stop("[create_Material] Error: Input value of 'OSL[,1]' has to be > 0.")
  }else if(!is.numeric(OSL[,2])){
    stop("[create_Material] Error: Input 'OSL[,2]' is not of type 'numeric'.")
  }else if(min(OSL[,2])<0){
    stop("[create_Material] Error: Input value of 'OSL[,2]' has to be >= 0.")
  }else if(!is.numeric(OSL[,3])){
    stop("[create_Material] Error: Input 'OSL[,3]' is not of type 'numeric'.")
  }else if(min(OSL[,3])<0){
    stop("[create_Material] Error: Input value of 'OSL[,3]' has to be >= 0.")
  }

  new.Material <- setMaterial(name = name,
                              description.TL = description.TL,
                              TL = TL,
                              description.OSL = description.OSL,
                              OSL=OSL)

  return(new.Material)
}
