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

    R1 <- filter1@reflexion
    R2 <- filter2@reflexion

    l <- filter1@transmission[,1]

    t1 <- filter1@transmission[,2]
    rd1 <- filter1@reference.thickness
    d1 <- filter1@thickness

    t2 <- filter2@transmission[,2]
    rd2 <- filter1@reference.thickness
    d2 <- filter1@thickness

    new.thickness <- d1 + d2
    new.reflexion <- max(R1, R2, na.rm = TRUE)

    L <- l

    if(filter1@name == filter2@name){
      T1 <- t1^(d1/rd1)
      T2 <- t2^(d2/rd2)

      Tf <- T1 * T2                  # Schott
      # T <- 1/(1/T1 + 1/T2 - 1)    # Semrock

    }else{
      T1 <- t1^(d1/rd1)
      T2 <- t2^(d2/rd2)

      Tf <- T1 * T2                  # Schott
      # T <- 1/(1/T1 + 1/T2 - 1)    # Semrock

    }

    new.transmission <- matrix(c(L, Tf),
                               nrow = length(L),
                               ncol = 2,
                               byrow = FALSE)

    new.filter <- setFilter(name = new.name,
                            description = new.description,
                            reference.thickness = new.thickness,
                            thickness = new.thickness,
                            reflexion = new.reflexion,
                            transmission = new.transmission)
  }

  return(new.filter)
          }
