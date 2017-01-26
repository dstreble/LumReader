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
#' @examples
#'
#' filter
#'
#' #create filters
#' filter1 <- default_Filters('example')[[1]]
#' filter2 <- default_Filters('example2')[[1]]
#'
#' new.filter <- combine_Filters (filter1, filter2)
#'
#' plot_Filter(new.filter)
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

    R1 <- filter1@reflexion
    R2 <- filter2@reflexion

    L <- filter1@transmission[,1]

    D1.r <- filter1@reference.thickness
    T1.r <- filter1@reference.transmission[,2]

    D1 <- filter1@thickness
    T1 <- filter1@transmission[,2]

    T2 <- filter2@transmission[,2]
    D2 <- filter1@thickness

    new.thickness <- D1 + D2
    new.reflexion <- max(R1, R2, na.rm = TRUE)

    if(filter1@name == filter2@name){
      Tf <- T1.r^(new.thickness/D1.r)

    }else{
      Tf <- T1 * T2                  # Schott
      # Tf <- 1/(1/T1 + 1/T2 - 1)    # Semrock

    }

    new.reference.transmission <- matrix(c(L, Tf),
                                         nrow = length(L),
                                         ncol = 2,
                                         byrow = FALSE)

    new.filter <- setFilter(name = new.name,
                            description = new.description,
                            reference.thickness = new.thickness,
                            thickness = new.thickness,
                            reflexion = new.reflexion,
                            reference.transmission = new.reference.transmission)
  }

  return(new.filter)
          }
