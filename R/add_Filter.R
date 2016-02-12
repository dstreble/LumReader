#' Function to add a filter to a stack
#'
#' This function adds a filter to a stack
#'
#' @param object
#'  \code{\linkS4class{FilterStack}} from which the filter has to be added
#'
#' @param filter
#'  \code{\linkS4class{Filter}} that has to be added
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#'
#' @export add_Filter

add_Filter <- function(

  object,

  filter

){
  if (missing(object)){
    stop("[add_Filter] Error: Input 'object' is missing.")
  }else if (!is(object,"FilterStack")){
    stop("[add_Filter] Error: Input 'object' is not of type 'FilterStack'.")
  }


  if (missing(filter)){
    stop("[add_Filter] Error: Input 'filter' is missing.")
  }else if (!is(filter,"Filter")){
    stop("[add_Filter] Error: Input 'filter' is not of type 'Filter'.")
  }

  new.list <- c(object@filters, filter)

  new.object <- setFilterStack(object@name,object@description,new.list)


  return(new.object)
}
