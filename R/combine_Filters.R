#' Function to combine two filters
#'
#' This function creates a new filter out of 2 filters.
#'
#' @param filter1
#'  \code{\linkS4class{Filter}} First of the two filters to combine.
#'
#' @param filter2
#'  \code{\linkS4class{Filter}} Second of the two filters to combine.
#'
#' @return
#'  The function creates a new \code{\linkS4class{Filter}} object.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export combine_Filters

combine_Filters <- function(

  filter1,

  filter2

){
  if (missing(filter1)){
    stop("[combine_Filters] Error: Input 'filter1' is missing.")
  }else if (!is(filter1,"Filter")){
    stop("[combine_Filters] Error: Input 'filter1' is not of type 'Filter'.")
  }

  if (missing(filter2)){
    stop("[combine_Filters] Error: Input 'filter2' is missing.")
  }else if (!is(filter2,"Filter")){
    stop("[combine_Filters] Error: Input 'filter2' is not of type 'Filter'.")
  }

  if(is.null(filter1@name)){
    new.filter <- filter2

  }else if(is.null(filter2@name)){
    new.filter <- filter1

  }else{
    new.name <- paste(filter1@name, "+", filter2@name, sep="")
    new.description <- paste("combination of:", filter1@name,"and", filter2@name)
    new.thickness <- filter1@thickness + filter2@thickness

    new.reflexion <- filter1@reflexion

    new.l <- filter1@transmission[,1]
    new.t <- filter1@transmission[,2] * filter2@reflexion * filter2@transmission[,2]

    new.transmission <- matrix(c(new.l,
                                 new.t),
                               nrow = 111,
                               ncol = 2,
                               byrow = FALSE)

    new.filter <- setFilter(name = new.name,
                            description = new.description,
                            thickness = new.thickness,
                            reflexion = new.reflexion,
                            transmission = new.transmission)
  }

  return(new.filter)
          }
