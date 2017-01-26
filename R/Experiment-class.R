#' Class Experiment
#'
#' Class \code{Experiment} contains the caracteristics of a luminescence experiment.
#'
#' @name Experiment-class
#' @rdname Experiment-class
#'
#' @slot name
#'  \link{character}: Name of the luminescence experiment.
#' @slot description
#'  \link{character}: Description of the experiment.
#' @slot reader
#'  \linkS4class{Reader}: TL/OSL reader unit used for the experiment.
#' @slot material
#'  \linkS4class{Material}: Material analysed by the experiment.
#' @slot type
#'  \link{character}: Type of experiment ('TL' or 'OSL'). By default, defined by the stimulation unit of the reader.
#' @slot interval
#'  \link{numeric}: vector defining the stimulation temperature (for 'TL') or wavelength (for 'OSL') interval on which the experiment focuses.
#' @slot emission
#'   \linkS4class{Stimulation}: Material emission spectra for the temperature or wavelength interval on which the experiment focuses.
#' @slot detected
#'   \linkS4class{Stimulation}: Emission spectra wich is detected by the reader.
#'
#' @author David Strebler
#'
#' @keywords classes
#'
#' @exportClass Experiment


##Class definition

setClass(Class = "Experiment",
         slots = c(name="character",
                   description="character",
                   reader="Reader",
                   material="Material",
                   type="character",
                   interval="numeric",
                   emission="Stimulation",
                   detected="Stimulation"),
         prototype = list(name = NULL,
                          description = "",
                          excitation= NULL,
                          material=NULL,
                          type="",
                          interval=NULL,
                          emission=NULL,
                          detected=NULL)
)

#Show method

#' @rdname Experiment-class
#' @aliases show show,Experiment-method
#'
setMethod(f = "show",
          signature = "Experiment",
          definition = function(object){
            cat("Name:", object@name, "\n")
            cat("Description:", object@description,"\n")

            if(!is.null(object@reader)){
              cat("Reader unit:", object@reader@name,"\n")
            }else{
              cat("no reader unit. \n")
            }

            if(!is.null(object@material)){
              cat("material:", object@material@name, "(TL:",object@material@description.TL, 'OSL:', object@material@description.OSL,")","\n")
            }else{
              cat("no Filters in the stack. \n")
            }
            if(!is.null(object@type)){
              cat("type:", object@type,"\n")

              if(!is.null(object@interval)){

                if(object@type=="TL"){
                  cat("Focus on the emission between:", min(object@interval), "and",max(object@interval), "[nm].","\n")
                }else if(object@type=="OSL"){
                  cat("Focus on the emission between:", min(object@interval), "and",max(object@interval), "[\u00b0C].","\n")
                }else{
                  cat("Focus on the emission between:", min(object@interval), "and",max(object@interval), "[?].","\n")
                }
              }else{
                  cat("no interval specified. \n")
                }
            }else{
              cat("Type of experiment not specified. \n")
            }

            if(!is.null(object@emission)){
              cat("Emission [nm ; a.u.]:", "\n")
              cat("\t ...from:", min(object@emission@emission[,1]), "to", max(object@emission@emission[,1]), "[nm]. \n")
              cat("\t ...between:", min(object@emission@emission[,2]), "and", max(object@emission@emission[,2]), "[a.u]. \n")
            }
          })

#Set method

## Generic
#' Method setExperiment
#'
#' @name Experiment-class
#' @rdname Experiment-class
#'
#' @param  name
#'  \link{character}: Name of the luminescence experiment.
#' @param description
#'  \link{character}: Description of the experiment.
#' @param reader
#'  \linkS4class{Reader}: TL/OSL reader unit used for the experiment.
#' @param material
#'  \linkS4class{Material}: Material analysed by the experiment.
#' @param type
#'  \link{character}: Type of experiment ('TL' or 'OSL'). By default, defined by the stimulation unit of the reader.
#' @param interval
#'  \link{numeric}: vector defining the stimulation temperature (for 'TL') or wavelength (for 'OSL') interval on which the experiment focuses.
#'
#' @exportMethod setExperiment

setGeneric(name="setExperiment",
           def=function(name,description,reader, material, type, interval){standardGeneric("setExperiment")}
)

## Method
#' @rdname Experiment-class
#' @aliases setExperiment setExperiment,Experiment-method

setMethod(f = "setExperiment",
          signature = c(name="character",
                        description="character",
                        reader="Reader",
                        material="Material",
                        type="character",
                        interval = "numeric"),
          definition = function(name,description,reader,material,type,interval){

            if(length(interval) != 2){
              stop("[setExperiment]: interval has to contain 2 elements.")
            }else if(interval[1]>interval[2]){
              stop("[setExperiment]: interval[1] has to be > to interval[2].")

            }
            if(!(type %in% c("TL", "OSL"))){
              stop("[setExperiment]: type can only be 'OSL' or 'TL'.")
            }

            # emission
            new.emission.name <- paste(name,"-emission",sep = "")

            if(type == "TL"){
              new.emission.description <- paste(type, "emission of", material@name, "for a stimulation between", interval[1], "and", interval[2], "[\u00b0C]")
            }else if(type == "OSL"){
              new.emission.description <- paste(type, "emission of", material@name, "for a stimulation between", interval[1], "and", interval[2], "[nm]")
            }else{
              new.emission.description <- description
            }

            new.emission.type <- type

            new.l <- seq(from=200,to=1000, by=10)

            if(type=="TL"){
              material.TL <- material@TL

              new.s <- rep(0,length(unique(material.TL[,1])))

              TL.wavelength <- unique(material.TL[,1])
              TL.temperatures <- unique(material.TL[,2])

              TL.signal <- matrix(data=material.TL[,3],
                                  nrow = length(TL.wavelength),
                                  ncol = length(TL.temperatures),
                                  byrow = TRUE)

              for(i in 1:length(TL.wavelength)){
                temp.signal <- TL.signal[i,]
                temp.signal.lim <- temp.signal[TL.temperatures >= interval[1] & TL.temperatures <= interval[2]]
                temp.mean <- mean(temp.signal.lim,na.rm = TRUE)

                new.s[i] <- temp.mean
              }

            }else if(type == "OSL"){
              material.OSL <- material@OSL
              new.s <- rep(1,length(unique(material.OSL[,1])))

              OSL.wavelength <- unique(material.OSL[,1])
              OSL.color <- unique(material.OSL[,2])

              OSL.signal <- matrix(data=material.OSL[,3],
                                  nrow = length(OSL.wavelength),
                                  ncol = length(OSL.color),
                                  byrow = TRUE)

              for(i in 1:length(OSL.wavelength)){
                temp.signal <- OSL.signal[i,]
                temp.signal.lim <- temp.signal[OSL.color >= interval[1] & OSL.color <= interval[2]]
                temp.mean <- mean(temp.signal.lim,na.rm = TRUE)

                new.s[i] <- temp.mean
              }
            }else{
              stop("[setExperiment]: type can only be 'OSL' or 'TL'.")
            }

            new.emission.signal <- matrix(c(new.l, new.s),
                                          nrow = length(new.l),
                                          ncol = 2,
                                          byrow = FALSE)

            new.emission <- new("Stimulation")
            new.emission@name <- new.emission.name
            new.emission@description <- new.emission.description
            new.emission@type <- new.emission.type
            new.emission@emission <- new.emission.signal


            new.detected.name <-paste(new.emission.name,"-detected", sep="")
            new.detected.description <- paste("Detected signal")
            new.detected.type <- new.emission.type
            new.detected.signal <- new.emission.signal
            new.detected.signal[,2] <- new.detected.signal[,2] * (reader@detection@efficiency[,2] / max(reader@detection@efficiency[,2]))

            new.detected <- new("Stimulation")
            new.detected@name <- new.detected.name
            new.detected@description <- new.detected.description
            new.detected@type <- new.detected.type
            new.detected@emission <- new.detected.signal


            new.object <- new("Experiment")
            new.object@name <- name
            new.object@description <- description
            new.object@reader <- reader
            new.object@material <- material
            new.object@type <- type
            new.object@interval <- interval
            new.object@emission <- new.emission
            new.object@detected <- new.detected

            return(new.object)
          })

#Get Method


## Generic
#' Method getExperiment
#'
#' @name Experiment-class
#' @rdname Experiment-class
#'
#' @param object
#'  \linkS4class{Experiment}: Experiment.
#' @param ref
#'  \link{character}: Slot reference.
#'
#' @exportMethod getExperiment
setGeneric(name = "getExperiment",
           def = function(object, ref){standardGeneric("getExperiment")}
)

## Method
#' @rdname Experiment-class
#' @aliases getExperiment getExperiment,Experiment-method

setMethod(f = "getExperiment",
          signature=c(object = "Experiment",
                      ref = "character"),
          definition = function(object, ref) {

            ref <- tolower(ref)

            if(ref == "name"){
              return(object@name)

            }else if(ref == "description"){
              return(object@description)

            }else if(ref == "reader"){
              return(object@reader)

            }else if(ref == "material"){
              return(object@material)

            }else if(ref == "type"){
              return(object@type)

            }else if(ref == "interval"){
              return(object@interval)

            }else if(ref == "result"){
              return(object@result)

            }else{
              return(object)
            }
          })
