#' Class Material
#'
#' Class \code{Material} contains the luminescence properties of a material.
#'
#' @name Material-class
#' @rdname Material-class
#'
#' @slot name
#'  \link{character}: Name of the material.
#' @slot description.TL
#'  \link{character}: Description of the TL properties of the material.
#' @slot description.OSL
#'  \link{character}: Description of the OSL properties of the material.
#' @slot TL
#'  \link{matrix}: TL emission properties of the material. The first column contains the emission wavelength [nm], the second column contains the stimuation temperature [°C] and the third column contains the emission intensity  [a.u].
#' @slot OSL
#'  \link{matrix}: OSL emission properties of the material. The first column contains the emission wavelength [nm], the second column contains the stimuation wavelength [nm] and the third column contains the emission intensity  [a.u].
#'
#'
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @exportClass Material


##Class definition

setClass(Class = "Material",
         slots = c(name="character",
                   description.TL="character",
                   description.OSL="character",
                   TL="matrix",
                   OSL="matrix"),
         prototype = list(name = NULL,
                          description.TL = "",
                          description.OSL = "",
                          TL = matrix(data=c(rep(seq(200,1000,10), each=81),
                                             rep(seq(0,800,10), times=81),
                                             rep(1,6561)),
                                      nrow = 6561,
                                      ncol = 3,
                                      byrow = FALSE),
                          OSL = matrix(data=c(rep(seq(200,1000,10), each=81),
                                              rep(seq(200,1000,10), times=81),
                                              rep(1,6561)),
                                      nrow = 6561,
                                      ncol = 3,
                                      byrow = FALSE)
                          )
)

#Show method
setMethod(f = "show",
          signature = "Material",
          definition = function(object){
            cat("Name:", object@name, "\n")
            cat("Description of the TL properties:", object@description.TL,"\n")
            cat("TL emission [nm ; \u00b0C ; a.u.]:", "\n")
            cat("\t ...from:", min(object@TL[,1]), "to", max(object@TL[,1]), "[nm]. \n")
            cat("\t ...and:", min(object@TL[,2]), "and", max(object@TL[,2]), "[\u00b0C]. \n")
            cat("\t ...between:", min(object@TL[,3]), "and", max(object@TL[,3]), "[a.u.]. \n")
            cat("Description of the OSL properties:", object@description.OSL,"\n")
            cat("OSL emission [nm ; \u00b0C ; a.u.]:", "\n")
            cat("\t ...from:", min(object@OSL[,1]), "to", max(object@OSL[,1]), "[nm]. \n")
            cat("\t ...and:", min(object@OSL[,2]), "and", max(object@OSL[,2]), "[\u00b0C]. \n")
            cat("\t ...between:", min(object@OSL[,3]), "and", max(object@OSL[,3]), "[a.u.]. \n")
          })

#Set method

## Generic
#' Method setMaterial
#'
#' @name Material-class
#' @rdname Material-class
#'
#' @param name
#'  \link{character}: Name of the material.
#' @param description.TL
#'  \link{character}: Description of the TL properties of the material.
#' @param description.OSL
#'  \link{character}: Description of the OSL properties of the material.
#' @param TL
#'  \link{matrix}: TL emission properties of the material. The first column contains the emission wavelength [nm], the second column contains the stimuation temperature [°C] and the third column contains the emission intensity  [a.u].
#' @param OSL
#'  \link{matrix}: OSL emission properties of the material. The first column contains the emission wavelength [nm], the second column contains the stimuation wavelength [nm] and the third column contains the emission intensity  [a.u].
#'
#' @exportMethod setMaterial

setGeneric(name="setMaterial",
           def=function(name,description.TL,description.OSL,TL,OSL){standardGeneric("setMaterial")}
)

## Method
#' @rdname Material-class
#' @aliases setMaterial setMaterial,Material-method

setMethod(f = "setMaterial",
          signature = c(name="character",
                        description.OSL="character",
                        description.TL="character",
                        TL="matrix",
                        OSL="matrix"),
          definition = function(name,description.TL,description.OSL,TL,OSL){

            if(!is.numeric(TL[,1])){
              stop("[setMaterial] Error: TL[,1] have to be of type 'numeric'.")
            }

            if(!is.numeric(TL[,2])){
              stop("[setMaterial] Error: TL[,2] have to be of type 'numeric'.")
            }
            if(!is.numeric(TL[,3])){
              stop("[setMaterial] Error: TL[,3] have to be of type 'numeric'.")
            }else if(min(TL[,3])<0){
              stop("[setMaterial] Error: TL[,3] have to be >= 0.")
            }else if(min(TL[,3])>1){
              stop("[setMaterial] Error: TL[,3] have to be <= 1.")
            }

            if(!is.numeric(OSL[,2])){
              stop("[setMaterial] Error: TL[,2] have to be of type 'numeric'.")
            }
            if(!is.numeric(OSL[,3])){
              stop("[setMaterial] Error: TL[,3] have to be of type 'numeric'.")
            }else if(min(OSL[,3])<0){
              stop("[setMaterial] Error: TL[,3] have to be >= 0.")
            }else if(min(OSL[,3])>1){
              stop("[setMaterial] Error: TL[,3] have to be <= 1.")
            }

            # TL

            TL.wavelength <- unique(TL[,1])
            TL.temperatures <- unique(TL[,2])
            TL.signal <- TL[,3]

            if(length(TL.signal) != length(TL.wavelength)*length(TL.temperatures)){
              stop("[setMaterial] Error: Length TL[,3] != length(TL.wavelength)*length(TL.temperatures).")
            }

            new.TL.wavelength <- seq(200,1000,10)
            new.TL.temperatures <- seq(0,800,10)

            new.TL.signal <- vector(mode = "numeric",length = length(new.TL.wavelength)*length(new.TL.temperatures))

            for(i in 1: length(new.TL.wavelength)){
              for(j in 1: length(new.TL.temperatures)){

                w.inf <- sum(TL.wavelength <= new.TL.wavelength[i])
                w.sup <- length(TL.wavelength) - sum(TL.wavelength > new.TL.wavelength[i])

                t.inf <- sum(TL.temperatures <= new.TL.temperatures[j])
                t.sup <- length(TL.temperatures) - sum(TL.temperatures > new.TL.temperatures[j])

                if(w.inf <= 0){
                  w.inf <- 1
                }
                if(w.sup <= 0){
                  w.sup <- 1
                }

                if(t.inf <= 0){
                  t.inf <- 1
                }
                if(t.sup <= 0){
                  t.sup <- 1
                }

                k <- (i-1)*length(new.TL.temperatures)+j

                up.left <- (w.inf-1)*length(TL.temperatures)+t.inf
                up.right <- (w.inf-1)*length(TL.temperatures)+t.sup
                down.left <- (w.sup-1)*length(TL.temperatures)+t.inf
                down.right <- (w.sup-1)*length(TL.temperatures)+t.sup

                new.TL.signal[k] <- mean(TL.signal[up.left],TL.signal[up.right],TL.signal[down.left],TL.signal[down.right])
              }
            }

            new.TL.signal <- new.TL.signal/max(new.TL.signal)

            new.TL <- matrix(data=c(rep(new.TL.wavelength, each=length(new.TL.temperatures)),
                                    rep(new.TL.temperatures,times=length(new.TL.wavelength)),
                                    new.TL.signal),
                             nrow = length(new.TL.signal),
                             ncol = 3,
                             byrow = FALSE)

            # OSL
            OSL.wavelength <- unique(OSL[,1])
            OSL.color <- unique(OSL[,2])
            OSL.signal <- OSL[,3]

            if(length(OSL.signal) != length(OSL.wavelength)*length(OSL.color)){
              stop("[setMaterial] Error: Length OSL[,3] != length(OSL.wavelength)*length(OSL.color).")
            }

            new.OSL.wavelength <- seq(200,1000,10)
            new.OSL.color <- seq(200,1000,10)

            new.OSL.signal <- vector(mode = "numeric",
                                     length = length(new.OSL.wavelength)*length(new.OSL.color))

            for(i in 1: length(new.OSL.wavelength)){
              for(j in 1: length(new.OSL.color)){

                w.inf <- sum(OSL.wavelength <= new.OSL.wavelength[i])
                w.sup <- length(OSL.wavelength) - sum(OSL.wavelength > new.OSL.wavelength[i])

                c.inf <- sum(OSL.color <= new.OSL.color[j])
                c.sup <- length(OSL.color) - sum(OSL.color > new.OSL.color[j])

                if(w.inf <= 0){
                  w.inf <- 1
                }
                if(w.sup <= 0){
                  w.sup <- 1
                }

                if(c.inf <= 0){
                  c.inf <- 1
                }
                if(c.sup <= 0){
                  c.sup <- 1
                }

                k <- (i-1)*length(new.OSL.color)+j

                up.left <- (w.inf-1)*length(OSL.color)+c.inf
                up.right <- (w.inf-1)*length(OSL.color)+c.sup
                down.left <- (w.sup-1)*length(OSL.color)+c.inf
                down.right <- (w.sup-1)*length(OSL.color)+c.sup

                new.OSL.signal[k] <- mean(OSL.signal[up.left],OSL.signal[up.right],OSL.signal[down.left],OSL.signal[down.right])
              }
            }

            new.OSL.signal <- new.OSL.signal/max(new.OSL.signal)

            new.OSL <- matrix(data=c(rep(new.OSL.wavelength, each=length(new.OSL.color)),
                                     rep(new.OSL.color,times=length(new.OSL.wavelength)),
                                     new.OSL.signal),
                             nrow = length(new.OSL.signal),
                             ncol = 3,
                             byrow = FALSE)

            new.object <- new("Material")

            new.object@name <- name
            new.object@description.TL <- description.TL
            new.object@description.OSL <- description.OSL
            new.object@TL <- new.TL
            new.object@OSL <- new.OSL

            return(new.object)
          })

#Get Method

## Generic
#' Method getMaterial
#'
#' @name Material-class
#' @rdname Material-class
#'
#' @param object
#'  \linkS4class{Material}: Material.
#' @param ref
#'  \link{character}: Material slot.
#'
#' @exportMethod getMaterial

setGeneric(name = "getMaterial",
           def = function(object, ref){standardGeneric("getMaterial")}
)

## Method
#' @rdname Material-class
#' @aliases getMaterial getMaterial,Material-method

setMethod(f = "getMaterial",
          signature=c(object = "Material",
                      ref = "character"),
          definition = function(object, ref) {

            tolower(ref)

            if(ref == "name"){
              return(object@name)

            }else if(ref == "description.TL"){
              return(object@description.TL)

            }else if(ref == "description.OSL"){
              return(object@description.OSL)

            }else if(ref == "TL"){
              return(object@TL)

            }else if(ref == "OSL"){
              return(object@OSL)

            }else if(ref == "l"){
              return(object@TL[,1])
            }else if(ref == "t"){
              return(object@TL[,2])
            }else if(ref == "c"){
              return(object@OSL[,2])
            }else{
              return(object)
            }
          })
