#' Class \code{Reader}
#'
#' Object class containing the properties of aPhotomultiplier tube.
#'
#' @name Reader-class
#' @rdname Reader-class
#'
#' @aliases
#'  Reader-class
#'  show,Reader-method
#'  setReader,Reader-method
#'  getReader,Reader-method
#'
#' @docType class
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @import methods
#'
#' @exportClass Reader


##Class definition

setClass(Class = "Reader",
         slots = c(name="character",
                   description="character",
                   stimulation="Stimulation",
                   filterStack="FilterStack",
                   PMT="PMT",
                   detection="PMT"),
         prototype = list(name = NULL,
                          description = NULL,
                          excitation= NULL,
                          filterStack=NULL,
                          PMT=NULL,
                          detection=NULL)
)

#Show method
setMethod(f = "show",
          signature = "Reader",
          definition = function(object){
            cat("Name:", object@name, "\n")
            cat("Description:", object@description,"\n")

            if(!is.null(object@stimulation)){
              cat("Stimulation unit:", object@stimulation@name,"\n")
            }else{
              cat("no stimulation unit. \n")
            }

            if(!is.null(object@filterStack)){
              cat("filterStack:", object@filterStack@name, "(",object@filterStack@description, ")","\n")
            }else{
              cat("no Filters in the stack. \n")
            }
            if(!is.null(object@PMT)){
              cat("PMT:", object@PMT@name, "(",object@PMT@description, ")","\n")
            }else{
              cat("no PMT. \n")
            }
            if(!is.null(object@detection)){
              cat("PMT:", object@detection@name, "(",object@detection@description, ")","\n")
            }else{
              cat("no detection windows. \n")
            }

          })

#Set method
setGeneric(name="setReader",
           def=function(name,description,stimulation, filterStack, PMT){standardGeneric("setReader")}
)

setMethod(f = "setReader",
          signature = c(name="character",
                        description="character",
                        stimulation="Stimulation",
                        filterStack="FilterStack",
                        PMT="PMT"),
          definition = function(name,description,stimulation,filterStack,PMT){

            new.detection <- new("PMT")
            new.detection@name <- "Detection windows"

            if(!(is.null(PMT) || is.null(filterStack))){
              new.detection@description <- paste(PMT@name, "with", filterStack@result@name)

              new.efficiency <- PMT@efficiency
              new.efficiency[,2] <- PMT@efficiency[,2] * filterStack@result@reflexion * filterStack@result@transmission[,2]

              new.detection@efficiency <- new.efficiency
            }

            new.object <- new("Reader")
            new.object@name <- name
            new.object@description <- description
            new.object@stimulation <- stimulation
            new.object@filterStack <- filterStack
            new.object@PMT <- PMT
            new.object@detection <- new.detection

            return(new.object)
          })

#Get Method

setGeneric(name = "getReader",
           def = function(object, ref){standardGeneric("getReader")}
)

setMethod(f = "getReader",
          signature=c(object = "Reader",
                      ref = "character"),
          definition = function(object, ref) {

            tolower(ref)

            if(ref == "name"){
              return(object@name)

            }else if(ref == "description"){
              return(object@description)

            }else if(ref == "stimulation"){
              return(object@stimulation)

            }else if(ref == "filterStack"){
              return(object@filterStack)

            }else if(ref == "PMT"){
              return(object@PMT)

            }else if(ref == "detection"){
              return(object@detection)

            }else{
              return(object)
            }
          })
