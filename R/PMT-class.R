#' Class \code{PMT}
#'
#' Object class containing the properties of aPhotomultiplier tube.
#'
#' @name PMT-class
#' @rdname PMT-class
#'
#' @aliases
#'  PMT-class
#'  show,PMT-method
#'  setPMT,PMT-method
#'  getPMT,PMT-method
#'
#' @docType class
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @import methods
#'
#' @exportClass PMT


##Class definition

setClass(Class = "PMT",
         slots = c(name="character",
                   description="character",
                   efficiency="matrix"),
         prototype = list(name = NULL,
                          description = "",
                          efficiency = matrix(data=c(seq(100,1200,10),
                                                       rep(1,111)),
                                                nrow = 111,
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
setGeneric(name="setPMT",
           def=function(name,description,efficiency){standardGeneric("setPMT")}
)

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

            new.l<- seq(from=100,to=1200, by=10)
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
                                       nrow = 111,
                                       ncol = 2,
                                       byrow = FALSE)

            new.object <- new("PMT")

            new.object@name <- name
            new.object@description <- description
            new.object@efficiency <- new.efficiency

            return(new.object)
          })

#Get Method

setGeneric(name = "getPMT",
           def = function(object, ref){standardGeneric("getPMT")}
)

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
