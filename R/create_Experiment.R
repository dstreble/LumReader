#' Function to create a Experiment.
#'
#' This function create a new Experiment.
#'
#' @param name
#'  \link{character}: Name of the Experiment.
#'
#' @param description
#'  \link{character}: Description of the Experiment.
#'
#' @param reader
#'  \code{\linkS4class{Reader}}: Reader used for the Experiment.
#'
#' @param material
#'  \code{\linkS4class{Material}}: Material used for the Experiment.
#'
#' @param type
#'  \link{character}: type of experiment ('TL' or 'OSL').
#'
#' @param interval
#'  \link{numeric}: Temperature or wavelength on which the experiment focuses on.
#'
#' @return
#'  This function return a new Experiment.
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @examples
#' # Create info
#' name <- 'example'
#' description <- 'example'
#'
#' # Create reader components
#' filter <- default_Filters('example')
#' filterStack <- create_FilterStack(name, description, filter)
#' stimulation <- default_Stimulation('example')
#' PMT <- default_PMT('example')
#'
#' # Create reader
#' reader <- create_Reader(name, description, stimulation, filterStack, PMT)
#'
#' # Create material
#' material <- default_Material('example')
#'
#' experiment <- create_Experiment(name, description,reader,material, 'OSL')
#'
#' plot_Experiment(experiment)
#'
#' @export create_Experiment

create_Experiment <- function(

  name,

  description,

  reader,

  material,

  type=NULL,

  interval=NULL
){

  if (missing(name)){
    stop("[create_Experiment] Error: Input 'name' is missing.")
  }else if (!is.character(name)){
    stop("[create_Experiment] Error: Input 'name' is not of type 'character'.")
  }
  if (missing(description)){
    stop("[create_Experiment] Error: Input 'description' is missing.")
  }else if (!is.character(description)){
    stop("[create_Experiment] Error: Input 'description' is not of type 'character'.")
  }

  if (missing(reader)){
    stop("[create_Experiment] Error: Input 'reader' is missing.")
  }else if (!is(reader,"Reader")){
    stop("[create_Experiment] Error: Input 'reader' is not of type 'Reader'.")
  }

  if(missing(material)){
    stop("[create_Experiment] Error: Input 'material' is missing.")
  }else if (!is(material,"Material")){
    stop("[create_Experiment] Error: Input 'material' is not of type 'Material'.")
  }

  if(is.null(type)){
    type <- reader@stimulation@type
  }else if (!is.character(type)){
    stop("[create_Experiment] Error: Input 'type' is not of type 'character'.")
  }else if(!(type %in% c("TL","OSL"))){
    stop("[create_Experiment] Error: Input 'type' can only be 'TL' or 'OSL.")
  }

  if(is.null(interval)){
    if(type == "TL"){
      peak.temperature <- 360
      interval <- c(peak.temperature-40,peak.temperature+40)

    }else if(type == "OSL"){
      stimulation <- reader@stimulation@emission
      peak.max <- max(stimulation[,2])
      peak.wavelength <- mean(stimulation[stimulation[,2]== peak.max, 1])

      interval <- c(peak.wavelength-20,peak.wavelength+20)
    }

  }else if(length(interval) == 1){
    if(interval < 0){
      stop("[create_Experiment] Error: Input 'interval' can not be < 0.")
    }

    if(type=="TL"){

      if(interval > max(material@TL[,2])){
        interval <-max(material@TL[,2])
      }

      peak.pos <- interval
      interval <- c(peak.pos-20,peak.pos+20)

    }else if(type=="OSL"){

      if(interval > max(material@OSL[,2])){
        interval <-max(material@OSL[,2])
      }

      peak.pos <- interval
      interval <- c(peak.pos-20,peak.pos+20)
    }

  }else if(length(interval) == 2){
    if(min(interval) < 0){
      stop("[create_Experiment] Error: Input 'interval' can not be < 0.")

    }else if(interval[1] > interval[2]){
      stop("[create_Experiment] Error: interval[1] must be < interval[2].")
    }
  }else{
    stop("[create_Experiment] Error: Input 'interval' can not include more than 2 elements.")
  }

  if(type=="TL"){
    if(interval[1] < min(material@TL[,2])){
      interval[1] <- min(material@TL[,2])
    }

    if(interval[2] < min(material@TL[,2])){
      interval[2] <- min(material@TL[,2]) + 10
    }

    if(interval[1] > max(material@TL[,2])){
      interval[1] <- max(material@TL[,2]) - 10
    }

    if(interval[2] > max(material@TL[,2])){
      interval[2] <- max(material@TL[,2])
    }

  }else if(type=="OSL"){
    if(interval[1] < min(material@OSL[,2])){
      interval[1] <- min(material@OSL[,2])
    }

    if(interval[2] < min(material@OSL[,2])){
      interval[2] <- min(material@OSL[,2]) + 10
    }

    if(interval[1] > max(material@OSL[,2])){
      interval[1] <- max(material@OSL[,2]) - 10
    }

    if(interval[2] > max(material@OSL[,2])){
      interval[2] <- max(material@OSL[,2])
    }
  }

  new.Experiment <- setExperiment(name=name,
                          description=description,
                          reader=reader,
                          material=material,
                          type=type,
                          interval=interval)

  return(new.Experiment)
}
