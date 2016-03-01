#' Function to create a filter.
#'
#' This function create a new filter.
#'
#' @param name
#'  \link{character}: Name of the filter.
#'
#' @param description
#'  \link{character}: Description of the filter.
#'
#' @param reference.thickness
#'  \link{numeric}: Reference thickness of the filter.
#'
#' @param thickness
#'  \link{numeric}: Thickness of the filter (by default thickness = reference.thickness).
#'
#' @param reflexion
#'  \link{numeric}: Reflexion of the filter (1-P).
#'
#' @param transmission
#'  \link{numeric}: Transmission of the filter (T).
#'
#' @return
#'  This function return a new filter.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export create_Filter

create_Filter <- function(

  name,

  description,

  reference.thickness,

  thickness=NULL,

  reflexion,

  transmission
){

  if (missing(name)){
    stop("[create_Filter] Error: Input 'name' is missing.")
  }else if (!is.character(name)){
    stop("[create_Filter] Error: Input 'name' is not of type 'character'.")
  }
  if (missing(description)){
    stop("[create_Filter] Error: Input 'description' is missing.")
  }else if (!is.character(description)){
    stop("[create_Filter] Error: Input 'description' is not of type 'character'.")
  }

  if (missing(reference.thickness)){
    stop("[create_Filter] Error: Input 'reference.thickness' is missing.")
  }else if (!is.numeric(reference.thickness)){
    stop("[create_Filter] Error: Input 'reference.thickness' is not of type 'numeric'.")
  }else if(reference.thickness<=0){
    stop("[create_Filter] Error: Input 'reference.thickness' can not be <= 0.")
  }

  if (is.null(thickness)){
    thickness <- reference.thickness
  }else if (!is.numeric(thickness)){
    stop("[create_Filter] Error: Input 'thickness' is not of type 'numeric'.")
  }else if(thickness<=0){
    stop("[create_Filter] Error: Input 'thickness' can not be <= 0.")
  }

  if (missing(reflexion)){
    stop("[create_Filter] Error: Input 'reflexion' is missing.")
  }else if (!is.numeric(reflexion)){
    stop("[create_Filter] Error: Input 'reflexion' is not of type 'numeric'.")
  }else if(reflexion<=0){
    stop("[create_Filter] Error: Input 'reflexion' can not be <= 0.")
  }else if(reflexion>1){
    stop("[create_Filter] Error: Input 'reflexion' can not be > 0.")
  }

  if (missing(transmission)){
    stop("[create_Filter] Error: Input 'transmission' is missing.")
  }else if(!is.numeric(transmission[,1])){
    stop("[create_Filter] Error: Input 'transmission[,1]' is not of type 'numeric'.")
  }else if(min(transmission[,1])<=0){
    stop("[create_Filter] Error: Input value of 'transmission[,1]' has to be > 0.")
  }else if(!is.numeric(transmission[,2])){
    stop("[create_Filter] Error: Input 'transmission[,2]' is not of type 'numeric'.")
  }else if(min(transmission[,2])<0){
    stop("[create_Filter] Error: Input value of 'transmission[,2]' has to be >= 0.")
  }else if(min(transmission[,2])>1){
    stop("[create_Filter] Error: Input value of 'transmission[,2]' has to be < 1.")
  }


  new.Filter <- setFilter(name=name,
                         description=description,
                         reference.thickness=reference.thickness,
                         thickness=thickness,
                         reflexion=reflexion,
                         transmission=transmission)

  return(new.Filter)
}
