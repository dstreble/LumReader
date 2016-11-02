#' Class Filter
#'
#' Class \code{Filter} contains the properties of a optical filter.
#'
#' @name Filter-class
#' @rdname Filter-class
#'
#' @slot name
#'  \link{character}: Name of the filter
#' @slot description
#'  \link{character}: Description of the filter
#' @slot reference.thickness
#'  \link{numeric}: Reference thickness for the filter.
#' @slot thickness
#'  \link{numeric}: Actual filter thickness (by default, the reference thickness).
#' @slot reflexion
#'  \link{numeric}: Reflection of the filter (1-P) (between 0, which means that the signal is completely reflected, and 1, which means there is no reflection of the signal).
#' @slot reference.transmission
#'  \link{matrix}: Transmission matrix of the filter for the reference.thickness. The first column contains the wavelength [nm] and the second the transmission [0-1] at these wavelengths.
#' @slot transmission
#'  \link{matrix}: Transmission matrix of the filter. The first column contains the wavelength [nm] and the second the transmission [0-1] at these wavelengths.
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @exportClass Filter


##Class definition

setClass(Class = "Filter",
         slots = c(name="character",
                   description="character",
                   reference.thickness="numeric",
                   thickness="numeric",
                   reflexion="numeric",
                   reference.transmission="matrix",
                   transmission="matrix"),

         prototype = list(name = NULL,
                          description = "",
                          reference.thickness=1,
                          thickness = 1,
                          reflexion = 1,
                          reference.transmission = matrix(data=c(seq(200,1000,10),
                                                                 rep(1,81)),
                                                          nrow = 81,
                                                          ncol = 2,
                                                          byrow = FALSE),
                          transmission = matrix(data=c(seq(200,1000,10),
                                                       rep(1,81)),
                                                nrow = 81,
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
            cat("Transmission (reference):", "\n")
            cat("\t ...from:", min(object@reference.transmission[,1]), "to", max(object@reference.transmission[,1]), "[nm]. \n")
            cat("\t ...between:", min(object@reference.transmission[,2])*100, "and", max(object@reference.transmission[,2])*100, "[%]. \n")
          })

#Set method

## Generic
#' Method setFilter
#'
#' @name Filter-class
#' @rdname Filter-class
#'
#' @param name
#'  \link{character}: Name of the filter
#' @param description
#'  \link{character}: Description of the filter
#' @param reference.thickness
#'  \link{numeric}: Reference thickness for the filter.
#' @param thickness
#'  \link{numeric}: Actual filter thickness (by default, the reference thickness).
#' @param reflexion
#'  \link{numeric}: Reflection of the filter (1-P) (between 0, which means that the signal is completely reflected, and 1, which means there is no reflection of the signal).
#' @param reference.transmission
#'  \link{matrix}: Transmission matrix of the filter. The first column contains the wavelength [nm] and the second the transmission [0-1] at these wavelengths.
#'
#' @exportMethod setFilter

setGeneric(name="setFilter",
           def=function(name,description,reference.thickness,thickness,reflexion,reference.transmission){standardGeneric("setFilter")}
)

## Method
#' @rdname Filter-class
#' @aliases setFilter setFilter,Filter-method

setMethod(f = "setFilter",
          signature = c(name="character",
                        description="character",
                        reference.thickness="numeric",
                        thickness="numeric",
                        reflexion="numeric",
                        reference.transmission="matrix"),
          definition = function(name,description,reference.thickness,thickness,reflexion,reference.transmission){

            if(reference.thickness <= 0){
              stop("[set_Filter] Error: thickness have to be > 0.")
            }

            if(thickness <= 0){
              stop("[set_Filter] Error: thickness have to be > 0.")
            }else if(thickness < reference.thickness){
              warning("[set_Filter] warning: thickness should be > reference.thickness.")
            }

            if(reflexion <= 0 || reflexion>1){
              stop("[set_Filter] Error: thickness have to be > 0 and <= 1.")
            }

            if(!is.numeric(reference.transmission[,1])){
              stop("[set_Filter] Error: reference.transmission[,1] have to be of type 'numeric'.")
            }

            if(!is.numeric(reference.transmission[,2])){
              stop("[set_Filter] Error: reference.transmission[,2] have to be of type 'numeric'.")
            }

            l <- reference.transmission[,1]
            t <- reference.transmission[,2]

            L<- seq(from=200,to=1000, by=10)
            T1 <- vector()

            for(i in 1: length(L)){
              i.l.inf <- sum(l <= L[i])
              i.l.sup <- length(l) - sum(l > L[i])
              if(i.l.inf == 0){
                i.l.inf <- 1
              }
              if(i.l.sup == 0){
                i.l.sup <- 1
              }

              temp.t.inf <- t[i.l.inf]
              temp.t.sup <- t[i.l.sup]

              temp.t <- (temp.t.inf+temp.t.sup)/2

              T1[i] <- temp.t
            }

            new.reference.transmission <- matrix(c(L, T1),
                                                 nrow = length(L),
                                                 ncol = 2,
                                                 byrow = FALSE)

            if(thickness != reference.thickness){
              r <- reference.thickness
              d <- thickness

              T2 <- T1^(d/r)
            }else{
              T2 <- T1
            }

            new.transmission <- matrix(c(L, T2),
                                       nrow = length(L),
                                       ncol = 2,
                                       byrow = FALSE)

            new.object <- new("Filter")

            new.object@name <- name
            new.object@description <- description

            new.object@reference.thickness <- reference.thickness
            new.object@thickness <- thickness

            new.object@reflexion <- reflexion

            new.object@reference.transmission <- new.reference.transmission
            new.object@transmission <- new.transmission

            return(new.object)
          })

#Get Method

## Generic
#' Method getFilter
#'
#' @name Filter-class
#' @rdname Filter-class
#'
#' @param object
#'  \linkS4class{Filter}: Filter
#' @param ref
#'  \link{character}: Slot reference.
#'
#' @exportMethod getFilter


setGeneric(name = "getFilter",
           def = function(object, ref){standardGeneric("getFilter")}
           )

## Method
#' @rdname Filter-class
#' @aliases getFilter getFilter,Filter-method

setMethod(f = "getFilter",
          signature=c(object = "Filter",
                      ref = "character"),
          definition = function(object, ref) {

            tolower(ref)

            if(ref == "name"){
              return(object@name)

            }else if(ref == "description"){
              return(object@description)

            }else if(ref == "reference.thickness"){
              return(object@reference.thickness)

            }else if(ref == "thickness"){
              return(object@thickness)

            }else if(ref == "reflexion"){
              return(object@reflexion)

            }else if(ref == "reference.transmission"){
              return(object@reference.transmission)

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
