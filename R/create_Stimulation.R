#' Function to create a Stimulation.
#'
#' This function create a new Stimulation.
#'
#' @param name
#'  \link{character}: Name of the Stimulation.
#'
#' @param description
#'  \link{character}: Description of the Stimulation.
#'
#'
#' @param emission
#'  \link{numeric}: emission of the Stimulation [u.a].
#'
#' @return
#'  This function return a new Stimulation.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export create_Stimulation

create_Stimulation <- function(

  name,

  description,

  emission
){

  if (missing(name)){
    stop("[create_Stimulation] Error: Input 'name' is missing.")
  }else if (!is.character(name)){
    stop("[create_Stimulation] Error: Input 'name' is not of type 'character'.")
  }
  if (missing(description)){
    stop("[create_Stimulation] Error: Input 'description' is missing.")
  }else if (!is.character(description)){
    stop("[create_Stimulation] Error: Input 'description' is not of type 'character'.")
  }

  if (missing(emission)){
    stop("[create_Stimulation] Error: Input 'emission' is missing.")
  }else if(!is.numeric(emission[,1])){
    stop("[create_Stimulation] Error: Input 'emission[,1]' is not of type 'numeric'.")
  }else if(min(emission[,1])<=0){
    stop("[create_Stimulation] Error: Input value of 'emission[,1]' has to be > 0.")
  }else if(!is.numeric(emission[,2])){
    stop("[create_Stimulation] Error: Input 'emission[,2]' is not of type 'numeric'.")
  }else if(min(emission[,2])<0){
    stop("[create_Stimulation] Error: Input value of 'emission[,2]' has to be >= 0.")
  }


  new.Stimulation <- setStimulation(name=name,
                    description=description,
                    emission=emission)

  return(new.Stimulation)
}
