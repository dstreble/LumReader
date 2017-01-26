#' Tools to simulate a TL/OSL Reader
#'
#' A series of functions to estimate the detection windows of a luminescence reader based on the filters and the photomultiplier (PMT) selected.
#' These functions also allow to simulate a luminescence experiment based on the thermoluminesce (TL) or the optically stimulated luminescence (OSL) properties of a material.
#' For those only interested by filter combination, the package can provide the transmission property of a filter stack.
#' The user can used the filters, detection units, stimulation units and materials already included in the package or creates its own.
#'
#' @details
#'
#' \tabular{ll}{  Package: \tab TLdating \cr
#'                Type: \tab Package\cr
#'                Version: \tab 0.1.0 \cr
#'                Date: \tab 2017-01-25 \cr
#'                License: \tab GPL-3
#'             }
#'
#' @name LumReader-package
#' @aliases LumReader-package LumReader
#' @docType package
#'
#' @author
#'  \bold{Authors} \cr
#'  \tabular{ll}{  David Strebler, \tab University of Cologne, Germany
#'  }
#'
##  \bold{Beta-tester} \cr
##  ... \cr
#'
#'  \bold{Supervisor} \cr
#'  \tabular{ll}{  Helmut Brückner, \tab University of Cologne, Germany \cr
#'                 Dominik Brill, \tab University of Cologne, Germany
#'  }
#'
##  \bold{Support contact} \cr
##  \email{david.strebler@uni-koeln.de} \cr
#'
##  \bold{Bug reporting} \cr
##  \email{david.strebler@uni-koeln.de} \cr
#'
##  \bold{Project website} \cr
##  ... \cr
#'
#'  \bold{Project source code repository} \cr
#'  \url{https://github.com/dstreble/LumReader} \cr
#'
##  \bold{Related package projects}\cr
##    \url{http://www.r-luminescence.de} \cr
##    \url{http://cran.r-project.org/package=Luminescence}\cr
#'
#'  \bold{Package maintainer} \cr
#'  David Strebler, Geographisches Institut, Universitat zu Koeln, Cologne, Germany. \cr
#'  \email{david.strebler@uni-koeln.de}
#'
#'  \bold{Acknowledgement} \cr
#'  This project is realized in the context of the CRC 806 “Our Way to Europe” (\url{http://www.sfb806.uni-koeln.de/}) which is funded by the German Research foundation (DFG). \cr
#'
## @references
#'
#' @keywords package
#'
#' @import methods
#' @import shiny
#' @importFrom tools file_path_sans_ext
#' @importFrom grDevices heat.colors rainbow terrain.colors
#' @importFrom graphics abline axis legend lines mtext par plot polygon
#' @importFrom plotly plot_ly layout
#' @importFrom gridExtra grid.arrange
#' @importFrom lattice levelplot panel.levelplot panel.abline

NULL


