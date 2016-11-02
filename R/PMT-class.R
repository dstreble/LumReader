#' Class \code{PMT}
#'
#' Object class containing the properties of a photomultiplier tube (PMT).
#'
#' @name PMT-class
#' @rdname PMT-class
#'
#' @slot name
#'  \link{character}: Name of the PMT
#' @slot description
#'  \link{character}: Description of the PMT
#' @slot efficiency
#'  \link{matrix}: Quantum efficiency of the PMT. The first column contains the wavelength [nm] and the second column the corresponding quantum efficiency [0-1].
#'
#' @docType class
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @exportClass PMT


##Class definition

setClass(Class = "PMT",
         slots = c(name="character",
                   description="character",
                   efficiency="matrix"),
         prototype = list(name = NULL,
                          description = "",
                          efficiency = matrix(data=c(seq(200,1000,10),
                                                       rep(1,81)),
                                                nrow = 81,
                                                ncol = 2,
                                                byrow = FALSE))
)

#Show method
setMethod(f = "show",
          signature = "PMT",
          definition = function(object){
            cat("Name:", object@name, "\n")
            cat("Description:", object@description,"\n")
            cat("Quantum efficiency [nm ; mA/W]:", "\n")
            cat("\t ...from:", min(object@efficiency[,1]), "to", max(object@efficiency[,1]), "[nm]. \n")
            cat("\t ...between:", min(object@efficiency[,2])*100, "and", max(object@efficiency[,2])*100, "[%]. \n")
          })

#Set method

## Generic
#' Method setPMT
#'
#' @name PMT-class
#' @rdname PMT-class
#'
#' @param name
#'  \link{character}: Name of the PMT
#' @param description
#'  \link{character}: Description of the PMT
#' @param efficiency
#'  \link{matrix}: Quantum efficiency of the PMT. The first column contains the wavelength [nm] and the second column the corresponding quantum efficiency [0-1].
#'
#' @exportMethod setPMT

setGeneric(name="setPMT",
           def=function(name,description,efficiency){standardGeneric("setPMT")}
)

## Method
#' @rdname PMT-class
#' @aliases setPMT setPMT,PMT-method

setMethod(f = "setPMT",
          signature = c(name="character",
                        description="character",
                        efficiency="matrix"),
          definition = function(name,description,efficiency){

            if(!is.numeric(efficiency[,1])){
              stop("[setPMT] Error: efficiency[,1] have to be of type 'numeric'.")
            }

            if(!is.numeric(efficiency[,2])){
              stop("[setPMT] Error: efficiency[,2] have to be of type 'numeric'.")
            }else if(min(efficiency[,2])<0){
              stop("[setPMT] Error: efficiency[,2] have to be >= 0.")
            }else if(min(efficiency[,2])>1){
              stop("[setPMT] Error: efficiency[,2] have to be <= 1.")
            }

            l <- efficiency[,1]
            s <- efficiency[,2]

            new.l<- seq(from=200,to=1000, by=10)
            new.s <- vector()

            for(i in 1: length(new.l)){
              i.l.inf <- sum(efficiency[,1] <= new.l[i])
              i.l.sup <- length(efficiency[,1]) - sum(efficiency[,1] > new.l[i])
              if(i.l.inf == 0){
                i.l.inf <- 1
              }
              if(i.l.sup == 0){
                i.l.sup <- 1
              }

              temp.s.inf <- efficiency[i.l.inf,2]
              temp.s.sup <- efficiency[i.l.sup,2]

              temp.s <- (temp.s.inf+temp.s.sup)/2


              new.s[i] <- temp.s
            }

            new.efficiency <- matrix(c(new.l, new.s),
                                       nrow = length(new.l),
                                       ncol = 2,
                                       byrow = FALSE)

            new.object <- new("PMT")

            new.object@name <- name
            new.object@description <- description
            new.object@efficiency <- new.efficiency

            return(new.object)
          })

#Get Method

## Generic
#' Method getPMT
#'
#' @name PMT-class
#' @rdname PMT-class
#'
#' @param object
#'  \linkS4class{PMT}: PMT.
#' @param ref
#'  \link{character}: Material slot.
#'
#' @exportMethod getPMT

setGeneric(name = "getPMT",
           def = function(object, ref){standardGeneric("getPMT")}
)

## Method
#' @rdname PMT-class
#' @aliases getPMT getPMT,PMT-method

setMethod(f = "getPMT",
          signature=c(object = "PMT",
                      ref = "character"),
          definition = function(object, ref) {

            tolower(ref)

            if(ref == "name"){
              return(object@name)

            }else if(ref == "description"){
              return(object@description)

            }else if(ref == "efficiency"){
              return(object@efficiency)

            }else if(ref == "l"){
              return(object@efficiency[,1])

            }else if(ref == "s"){
              return(object@efficiency[,2])
            }else{
              return(object)
            }
          })
