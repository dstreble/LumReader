#' Class \code{Filter}
#'
#' Object class containing the properties of a optical filter.
#'
#' @name Filter-class
#' @rdname Filter-class
#'
#' @aliases
#'  Filter-class
#'  show,Filter-method
#'  setFilter,Filter-method
#'  getFilter,Filter-method
#'
#' @docType class
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @import methods
#'
#' @exportClass Filter


##Class definition

setClass(Class = "Filter",
         slots = c(name="character",
                   description="character",
                   reference.thickness="numeric",
                   thickness="numeric",
                   reflexion="numeric",
                   transmission="matrix"),
         prototype = list(name = NULL,
                          description = "",
                          reference.thickness=1,
                          thickness = 1,
                          reflexion = 1,
                          transmission = matrix(data=c(seq(100,1200,10),
                                                       rep(1,111)),
                                                nrow = 111,
                                                ncol = 2,
                                                byrow = FALSE))
        )

#Show method
setMethod(f = "show",
          signature = "Filter",
          definition = function(object){
            cat("Name:", object@name, "\n")
            cat("Description:", object@description,"\n")
            cat("Thickness (reference) [mm]: ", object@thickness, " (",object@reference.thickness , ") \n",sep = "")
            cat("Reflexion (1-P) [%]:", object@reflexion*100, "\n")
            cat("Transmission:", "\n")
            cat("\t ...from:", min(object@transmission[,1]), "to", max(object@transmission[,1]), "[nm]. \n")
            cat("\t ...between:", min(object@transmission[,2])*100, "and", max(object@transmission[,2])*100, "[%]. \n")
          })

#Set method
setGeneric(name="setFilter",
           def=function(name,description,reference.thickness,thickness,reflexion,transmission){standardGeneric("setFilter")}
)

setMethod(f = "setFilter",
          signature = c(name="character",
                        description="character",
                        reference.thickness="numeric",
                        thickness="numeric",
                        reflexion="numeric",
                        transmission="matrix"),
          definition = function(name,description,reference.thickness,thickness,reflexion,transmission){

            if(reference.thickness <= 0){
              stop("[set_Filter] Error: thickness have to be > 0.")
            }

            if(thickness <= 0){
              stop("[set_Filter] Error: thickness have to be > 0.")
            }

            if(reflexion <= 0 || reflexion>1){
              stop("[set_Filter] Error: thickness have to be > 0 and <= 1.")
            }

            if(!is.numeric(transmission[,1])){
              stop("[set_Filter] Error: transmission[,1] have to be of type 'numeric'.")
            }

            if(!is.numeric(transmission[,2])){
              stop("[set_Filter] Error: transmission[,2] have to be of type 'numeric'.")
            }

            new.object <- new("Filter")

            new.object@name <- name
            new.object@description <- description
            new.object@reference.thickness <- reference.thickness
            new.object@thickness <- thickness
            new.object@reflexion <- reflexion

            l <- transmission[,1]
            t <- transmission[,2]

            L<- seq(from=100,to=1200, by=10)
            T <- vector()

            for(i in 1: length(L)){
              i.l.inf <- sum(transmission[,1] <= L[i])
              i.l.sup <- length(transmission[,1]) - sum(transmission[,1] > L[i])
              if(i.l.inf == 0){
                i.l.inf <- 1
              }
              if(i.l.sup == 0){
                i.l.sup <- 1
              }

              temp.t.inf <- transmission[i.l.inf,2]
              temp.t.sup <- transmission[i.l.sup,2]

              temp.t <- (temp.t.inf+temp.t.sup)/2

              T[i] <- temp.t
            }

            # d <- thickness
            # r <- reference.thickness
            # T <- T^(d/r)

            new.transmission <- matrix(c(L, T),
                                       nrow = length(L),
                                       ncol = 2,
                                       byrow = FALSE)

            new.object@transmission <- new.transmission

            return(new.object)
          })

#Get Method

setGeneric(name = "getFilter",
           def = function(object, ref){standardGeneric("getFilter")}
           )

setMethod(f = "getFilter",
          signature=c(object = "Filter",
                      ref = "character"),
          definition = function(object, ref) {

            tolower(ref)

            if(ref == "name"){
              return(object@name)

            }else if(ref == "description"){
              return(object@description)

            }else if(ref == "reference thickness"){
              return(object@reference.thickness)

            }else if(ref == "thickness"){
              return(object@thickness)

            }else if(ref == "reflexion"){
              return(object@reflexion)

            }else if(ref == "transmission"){
              return(object@transmission)

            }else if(ref == "l"){
              return(object@transmission[,1])

            }else if(ref == "t"){
              return(object@transmission[,2])
            }else{
              return(object)
            }
          })
