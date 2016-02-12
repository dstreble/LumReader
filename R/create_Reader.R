#' Function to create a Reader.
#'
#' This function create a new Reader.
#'
#' @param name
#'  \link{character}: Name of the Reader.
#'
#' @param description
#'  \link{character}: Description of the Reader.
#'
#' @param stimulation
#'  \link{numeric}: Excitation source of the Reader.
#'
#' @param filterStack
#'  \link{numeric}: filter stack of the Reader.
#'
#' @param PMT
#'  \link{numeric}: PMT of the Reader.
#'
#' @return
#'  This function return a new Reader.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export create_Reader

create_Reader <- function(

  name,

  description,

  stimulation,

  filterStack,

  PMT
){

  if (missing(name)){
    stop("[create_Reader] Error: Input 'name' is missing.")
  }else if (!is.character(name)){
    stop("[create_Reader] Error: Input 'name' is not of type 'character'.")
  }
  if (missing(description)){
    stop("[create_Reader] Error: Input 'description' is missing.")
  }else if (!is.character(description)){
    stop("[create_Reader] Error: Input 'description' is not of type 'character'.")
  }

  if (missing(stimulation)){
    stop("[create_Reader] Error: Input 'stimulation' is missing.")
  }else if (!is(stimulation,"Stimulation")){
    stop("[create_Reader] Error: Input 'stimulation' is not of type 'Stimulation'.")
  }
  if (missing(filterStack)){
    stop("[create_Reader] Error: Input 'filterStack' is missing.")
  }else if (!is(filterStack,"FilterStack")){
    stop("[create_Reader] Error: Input 'filterStack' is not of type 'FilterStack'.")
  }

  if (missing(PMT)){
    stop("[create_Reader] Error: Input 'PMT' is missing.")
  }else if(!is(PMT,"PMT")){
    stop("[create_Reader] Error: Input 'PMT' is not of type 'PMT'.")
  }


  new.Reader <- setReader(name=name,
                          description=description,
                          stimulation=stimulation,
                          filterStack=filterStack,
                          PMT=PMT)

  return(new.Reader)
}
