#' Function to remove a filter from a stack
#'
#' This function removes a filter from a stack
#'
#' @param object
#'  \code{\linkS4class{FilterStack}} from which the filter has to be removed.
#'
#' @param filter
#'  \code{\linkS4class{Filter}} that has to be removed.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#'
#' @export remove_Filter

remove_Filter <- function(

  object,

  filter

){
  if (missing(object)){
    stop("[remove_Filter] Error: Input 'object' is missing.")
  }else if (!is(object,"FilterStack")){
    stop("[remove_Filter] Error: Input 'object' is not of type 'FilterStack'.")
  }


  if (missing(filter)){
    stop("[remove_Filter] Error: Input 'filter' is missing.")
  }else if (!is(filter,"Filter")){
    stop("[remove_Filter] Error: Input 'filter' is not of type 'Filter'.")
  }

  id.filter <- 0
  temp.list <- list()
  for(i in 1:length(object@filters)){
    temp.filter <- object@filters[[i]]
    if(temp.filter@name == filter@name){
      id.filter <- i
    }else{
      temp.list <- c(temp.list, temp.filter)
    }
  }

  if(id.filter == 0){
    warning("[remove_Filter] Warning: the list contains no filter", filter@name)
  }
  new.list <- temp.list

  new.object <- setFilterStack(object@name,object@description,new.list)

  return(new.object)
}
