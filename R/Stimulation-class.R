#' Class Stimulation
#'
#' Class \code{Stimulation} contains the properties of a stimulation unit.
#'
#'
#' @name Stimulation-class
#' @rdname Stimulation-class
#'
#' @slot name
#'  \link{character}: name of the stimulation unit.
#'
#' @slot description
#'  \link{character}: description of the stimulation unit.
#'
#' @slot type
#'  \link{character}: type of the stimulation ('TL' or 'OSL').
#'
#' @slot emission
#'  \link{matrix}: Emission spectra of the stimulation unit.
#'  The first column contains the wavelength [nm] and the second the intensity of the signal [a.u].
#'
#'
#' @author David Strebler
#'
#' @exportClass Stimulation

##Class definition

setClass(Class = "Stimulation",
         slots = c(name="character",
                   description="character",
                   type="character",
                   emission="matrix"),
         prototype = list(name = NULL,
                          description = "",
                          type="",
                          emission = matrix(data=c(seq(200,1000,10),
                                                     rep(1,81)),
                                              nrow = 81,
                                              ncol = 2,
                                              byrow = FALSE))
)

#Show method

setMethod(f = "show",
          signature = "Stimulation",
          definition = function(object){
            cat("Name:", object@name, "\n")
            cat("Description:", object@description,"\n")
            cat("Type:", object@type,"\n")
            cat("Emission [nm ; a.u.]:", "\n")
            cat("\t ...from:", min(object@emission[,1]), "to", max(object@emission[,1]), "[nm]. \n")
            cat("\t ...between:", min(object@emission[,2])*100, "and", max(object@emission[,2]), "[a.u]. \n")
          })


#Set method
# Generic
#' Method setStimulation
#'
#' @param  name
#'  \link{character}: name of the stimulation unit.
#'
#' @param description
#'  \link{character}: description of the stimulation unit.
#'
#' @param type
#'  \link{character}: type of the stimulation ('TL' or 'OSL').
#'
#' @param emission
#'  \link{matrix}: Emission spectra of the stimulation unit.
#'  The first column contains the wavelength [nm] and the second the intensity of the signal [a.u].
#'
#' @name Stimulation-class
#' @rdname Stimulation-class
#' @exportMethod setStimulation

setGeneric(name="setStimulation",
           def=function(name,description,type,emission){standardGeneric("setStimulation")}
)

# Method
#' @rdname Stimulation-class
#' @aliases setStimulation setStimulation,Stimulation-method

setMethod(f = "setStimulation",
          signature = c(name="character",
                        description="character",
                        type="character",
                        emission="matrix"),
          definition = function(name,description,type,emission){

            if(!is.character(type)){
              stop("[setStimulation] Error: type has to be of type 'character'.")
            }else if(!(type %in% c("TL", "OSL"))){
              stop("[setStimulation] Error: type can only le 'TL' or 'OSL'.")
            }

            if(!is.numeric(emission[,1])){
              stop("[setStimulation] Error: emission[,1] has to be of type 'numeric'.")
            }

            if(!is.numeric(emission[,2])){
              stop("[setStimulation] Error: emission[,2] has to be of type 'numeric'.")
            }else if(min(emission[,2])<0){
              stop("[setStimulation] Error: emission[,2] has to be >= 0.")
            }else if(min(emission[,2])>1){
              stop("[setStimulation] Error: emission[,2] has to be <= 1.")
            }

            l <- emission[,1]
            s <- emission[,2]

            new.l<- seq(from=200,to=1000, by=10)
            new.s <- vector()

            for(i in 1: length(new.l)){
              i.l.inf <- sum(emission[,1] <= new.l[i])
              i.l.sup <- length(emission[,1]) - sum(emission[,1] > new.l[i])
              if(i.l.inf == 0){
                i.l.inf <- 1
              }
              if(i.l.sup == 0){
                i.l.sup <- 1
              }

              temp.s.inf <- emission[i.l.inf,2]
              temp.s.sup <- emission[i.l.sup,2]

              temp.s <- (temp.s.inf+temp.s.sup)/2


              new.s[i] <- temp.s
            }

            new.emission <- matrix(c(new.l, new.s),
                                     nrow = length(new.l),
                                     ncol = 2,
                                     byrow = FALSE)

            new.object <- new("Stimulation")

            new.object@name <- name
            new.object@description <- description
            new.object@type <- type
            new.object@emission <- new.emission

            return(new.object)
          })

#Get Method

# Generic

#' Method getStimulation
#'
#' @name Stimulation-class
#' @rdname Stimulation-class
#'
#' @param object
#'  \linkS4class{Stimulation}: Stimulation unit
#' @param ref
#'  \link{character}: Slot reference.
#'
#' @exportMethod getStimulation

setGeneric(name = "getStimulation",
           def = function(object, ref){standardGeneric("getStimulation")}
)

# Method
#' @rdname Stimulation-class
#' @aliases getStimulation getStimulation,Stimulation-method

setMethod(f = "getStimulation",
          signature=c(object = "Stimulation",
                      ref = "character"),
          definition = function(object, ref) {

            tolower(ref)

            if(ref == "name"){
              return(object@name)

            }else if(ref == "description"){
              return(object@description)

            }else if(ref == "type"){
              return(object@type)

            }else if(ref == "emission"){
              return(object@emission)

            }else if(ref == "l"){
              return(object@emission[,1])

            }else if(ref == "s"){
              return(object@emission[,2])
            }else{
              return(object)
            }
          })
