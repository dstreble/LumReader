#' Function to create a filter stack
#'
#' This function create a new filter stack
#'
#' @param name
#'  \link{character}: Name of the filter stack.
#'
#' @param description
#'  \link{character}: Description of the filter stack.
#'
#' @param filters
#'  \link{list}: Filter included in the stack
#'
#' @return
#'  This function return a new filter stack.
#'
#' @examples
#' #Data
#' name <- "example"
#' description <- "non realistic filterStack"
#'
#' filters <- default_Filters(c('example','example2'))
#'
#' #Filterstack
#' filterstack <- create_FilterStack(name,description,filters)
#'
#' plot_FilterStack(filterstack)
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export create_FilterStack

create_FilterStack <- function(

  name,

  description,

  filters
){

  if (missing(name)){
    stop("[create_FilterStack] Error: Input 'name' is missing.")
  }else if (!is.character(name)){
    stop("[create_FilterStack] Error: Input 'name' is not of type 'character'.")
  }
  if (missing(description)){
    stop("[create_FilterStack] Error: Input 'description' is missing.")
  }else if (!is.character(description)){
    stop("[create_FilterStack] Error: Input 'description' is not of type 'character'.")
  }
  if (missing(filters)){
    stop("[create_FilterStack] Error: Input 'filters' is missing.")
  }else if (!is.list(filters)){
    stop("[create_FilterStack] Error: Input 'filters' is not of type 'list'.")
  }else if(length(filters)==0){
    stop("[create_FilterStack] Error: Input 'filters' does not contain any Filter.")
  }
  for(i in 1: length(filters)){
    if(!is(filters[[i]], "Filter")){
      stop("[create_FilterStack] Error: Input 'filters' does not only contain Filter object.")
    }
  }

  new.stack <- setFilterStack(name,description,filters)

  return(new.stack)
}
