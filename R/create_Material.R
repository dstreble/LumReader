#' Function to create a Material.
#'
#' This function create a new Material.
#'
#' @param name
#'  \link{character}: Name of the material.
#'
#' @param description.TL
#'  \link{character}: description of the material TL properties.
#'
#' @param TL
#'  \link{numeric}: TL response of the Material [a.u.].
#'
#' @param description.OSL
#'  \link{character}: description of the material OSL properties.
#'
#' @param OSL
#'  \link{numeric}: TL of the Material [a.u.].
#'
#' @return
#'  This function return a new Material.
#'
#' @examples
#' #Data
#' name <- "example"
#'
#' # TL
#' description.TL <- "example"
#'
#' TL.wavelength <- seq(200,1000,10)
#' TL.temperatures <- seq(0,800,10)
#'
#' # TL peak
#' TL.peak.x <- dnorm(TL.wavelength,400,50)
#' TL.peak.x <- rep(TL.peak.x,each=length(TL.temperatures))
#'
#' TL.peak.x <- TL.peak.x/max(TL.peak.x)
#'
#' TL.peak.y <- dnorm(TL.temperatures,400,25)
#' TL.peak.y <- rep(TL.peak.y,times=length(TL.wavelength))
#' TL.peak.y <- TL.peak.y/max(TL.peak.y)
#'
#' TL.signal <- TL.peak.x*TL.peak.y
#'
#' TL <- matrix(data=c(rep(TL.wavelength,
#'                         each=length(TL.temperatures)),
#'                     rep(TL.temperatures,
#'                         times=length(TL.wavelength)),
#'                     TL.signal),
#'              nrow = length(TL.signal),
#'              ncol = 3,
#'              byrow = FALSE)
#'
#' # OSL
#' description.OSL <- "example"
#'
#' OSL.wavelength <- seq(200,1000,10)
#' OSL.color <- seq(200,1000,10)
#'
#' OSL.peak.x <- dnorm(OSL.wavelength,300,100)
#' OSL.peak.x <- rep(OSL.peak.x,each=length(OSL.color))
#'
#' OSL.peak.x <- OSL.peak.x/max(OSL.peak.x)
#'
#' OSL.peak.y <- dnorm(OSL.color,500,50)
#' OSL.peak.y <- rep(OSL.peak.y,times=length(OSL.wavelength))
#' OSL.peak.y <- OSL.peak.y/max(OSL.peak.y)
#'
#' OSL.signal <- OSL.peak.x*OSL.peak.y
#'
#'
#' OSL <- matrix(data=c(rep(OSL.wavelength,
#'                          each=length(OSL.color)),
#'                      rep(OSL.color,
#'                          times=length(OSL.wavelength)),
#'                      OSL.signal),
#'               nrow = length(OSL.signal),
#'               ncol = 3,
#'               byrow = FALSE)
#'
#' # Material
#'
#' material <- create_Material(name = name,
#'                             description.TL = description.TL,
#'                             TL = TL,
#'                             description.OSL = description.OSL,
#'                             OSL = OSL)
#'
#' plot_Material(material)
#'
#' @author David Strebler, University of Cologne (Germany).
#'
#' @export create_Material

create_Material <- function(

  name,

  description.TL,

  description.OSL,

  TL,

  OSL
){

  if (missing(name)){
    stop("[create_Material] Error: Input 'name' is missing.")
  }else if (!is.character(name)){
    stop("[create_Material] Error: Input 'name' is not of type 'character'.")
  }

  if (missing(description.TL)){
    stop("[create_Material] Error: Input 'description.TL' is missing.")
  }else if (!is.character(description.TL)){
    stop("[create_Material] Error: Input 'description.TL' is not of type 'character'.")
  }

  if (missing(TL)){
    stop("[create_Material] Error: Input 'TL' is missing.")
  }else if(!is.numeric(TL[,1])){
    stop("[create_Material] Error: Input 'TL[,1]' is not of type 'numeric'.")
  }else if(min(TL[,1])<=0){
    stop("[create_Material] Error: Input value of 'TL[,1]' has to be > 0.")
  }else if(!is.numeric(TL[,2])){
    stop("[create_Material] Error: Input 'TL[,2]' is not of type 'numeric'.")
  }else if(min(TL[,2])<0){
    stop("[create_Material] Error: Input value of 'TL[,2]' has to be >= 0.")
  }else if(!is.numeric(TL[,3])){
    stop("[create_Material] Error: Input 'TL[,3]' is not of type 'numeric'.")
  }else if(min(TL[,3])<0){
    stop("[create_Material] Error: Input value of 'TL[,3]' has to be >= 0.")
  }

  if (missing(description.OSL)){
    stop("[create_Material] Error: Input 'description.OSL' is missing.")
  }else if (!is.character(description.OSL)){
    stop("[create_Material] Error: Input 'description.OSL' is not of type 'character'.")
  }

  if (missing(OSL)){
    stop("[create_Material] Error: Input 'OSL' is missing.")
  }else if(!is.numeric(OSL[,1])){
    stop("[create_Material] Error: Input 'OSL[,1]' is not of type 'numeric'.")
  }else if(min(OSL[,1])<=0){
    stop("[create_Material] Error: Input value of 'OSL[,1]' has to be > 0.")
  }else if(!is.numeric(OSL[,2])){
    stop("[create_Material] Error: Input 'OSL[,2]' is not of type 'numeric'.")
  }else if(min(OSL[,2])<0){
    stop("[create_Material] Error: Input value of 'OSL[,2]' has to be >= 0.")
  }else if(!is.numeric(OSL[,3])){
    stop("[create_Material] Error: Input 'OSL[,3]' is not of type 'numeric'.")
  }else if(min(OSL[,3])<0){
    stop("[create_Material] Error: Input value of 'OSL[,3]' has to be >= 0.")
  }

  new.Material <- setMaterial(name = name,
                              description.TL = description.TL,
                              TL = TL,
                              description.OSL = description.OSL,
                              OSL=OSL)

  return(new.Material)
}
