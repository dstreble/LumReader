#' Class \code{Stimulation}
#'
#' Object class containing the properties of aPhotomultiplier tube.
#'
#' @name Stimulation-class
#' @rdname Stimulation-class
#'
#' @aliases
#'  Stimulation-class
#'  show,Stimulation-method
#'  setStimulation,Stimulation-method
#'  getStimulation,Stimulation-method
#'
#' @docType class
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @import methods
#'
#' @exportClass Stimulation


##Class definition

setClass(Class = "Stimulation",
         slots = c(name="character",
                   description="character",
                   emission="matrix"),
         prototype = list(name = NULL,
                          description = "",
                          emission = matrix(data=c(seq(100,1200,10),
                                                     rep(1,111)),
                                              nrow = 111,
                                              ncol = 2,
                                              byrow = FALSE))
)

#Show method
setMethod(f = "show",
          signature = "Stimulation",
          definition = function(object){
            cat("Name:", object@name, "\n")
            cat("Description:", object@description,"\n")
            cat("Quantum emission [nm ; u.a]:", "\n")
            cat("\t ...from:", min(object@emission[,1]), "to", max(object@emission[,1]), "[nm]. \n")
            cat("\t ...between:", min(object@emission[,2])*100, "and", max(object@emission[,2])*100, "[%]. \n")
          })

#Set method
setGeneric(name="setStimulation",
           def=function(name,description,emission){standardGeneric("setStimulation")}
)

setMethod(f = "setStimulation",
          signature = c(name="character",
                        description="character",
                        emission="matrix"),
          definition = function(name,description,emission){

            if(!is.numeric(emission[,1])){
              stop("[setStimulation] Error: emission[,1] have to be of type 'numeric'.")
            }

            if(!is.numeric(emission[,2])){
              stop("[setStimulation] Error: emission[,2] have to be of type 'numeric'.")
            }else if(min(emission[,2])<0){
              stop("[setStimulation] Error: emission[,2] have to be >= 0.")
            }else if(min(emission[,2])>1){
              stop("[setStimulation] Error: emission[,2] have to be <= 1.")
            }

            l <- emission[,1]
            s <- emission[,2]

            new.l<- seq(from=100,to=1200, by=10)
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
                                     nrow = 111,
                                     ncol = 2,
                                     byrow = FALSE)

            new.object <- new("Stimulation")

            new.object@name <- name
            new.object@description <- description
            new.object@emission <- new.emission

            return(new.object)
          })

#Get Method

setGeneric(name = "getStimulation",
           def = function(object, ref){standardGeneric("getStimulation")}
)

setMethod(f = "getStimulation",
          signature=c(object = "Stimulation",
                      ref = "character"),
          definition = function(object, ref) {

            tolower(ref)

            if(ref == "name"){
              return(object@name)

            }else if(ref == "description"){
              return(object@description)

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
