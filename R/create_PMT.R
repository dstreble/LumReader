#' Function to create a PMT.
#'
#' This function create a new PMT.
#'
#' @param name
#'  \link{character}: Name of the PMT.
#'
#' @param description
#'  \link{character}: Description of the PMT.
#'
#'
#' @param efficiency
#'  \link{numeric}: efficiency of the PMT [mA/W].
#'
#' @return
#'  This function return a new PMT.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export create_PMT

create_PMT <- function(

  name,

  description,

  efficiency
){

  if (missing(name)){
    stop("[create_PMT] Error: Input 'name' is missing.")
  }else if (!is.character(name)){
    stop("[create_PMT] Error: Input 'name' is not of type 'character'.")
  }
  if (missing(description)){
    stop("[create_PMT] Error: Input 'description' is missing.")
  }else if (!is.character(description)){
    stop("[create_PMT] Error: Input 'description' is not of type 'character'.")
  }

  if (missing(efficiency)){
    stop("[create_PMT] Error: Input 'efficiency' is missing.")
  }else if(!is.numeric(efficiency[,1])){
    stop("[create_PMT] Error: Input 'efficiency[,1]' is not of type 'numeric'.")
  }else if(min(efficiency[,1])<=0){
    stop("[create_PMT] Error: Input value of 'efficiency[,1]' has to be > 0.")
  }else if(!is.numeric(efficiency[,2])){
    stop("[create_PMT] Error: Input 'efficiency[,2]' is not of type 'numeric'.")
  }else if(min(efficiency[,2])<0){
    stop("[create_PMT] Error: Input value of 'efficiency[,2]' has to be >= 0.")
  }


  new.PMT <- setPMT(name=name,
                    description=description,
                    efficiency=efficiency)

  return(new.PMT)
}
