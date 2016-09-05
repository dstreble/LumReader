#' Class \code{FilterStack}
#'
#' Object class containing a combination of filters.
#'
#' @name FilterStack-class
#' @rdname FilterStack-class
#'
#' @slot name
#'  \link{character}: Name of the filter stack.
#' @slot description
#'  \link{character}: Description of the filter stack.
#' @slot filters
#'  \link{list}: List of the \linkS4class{Filter} which are in the filter stack.
#' @slot bunch
#'  \linkS4class{Filter}: Properties of the complete filter stack.
#'
#' @author David Strebler
#'
#'
#' @exportClass FilterStack

##Class definition

setClass(Class = "FilterStack",
         slots = c(name="character",
                   description="character",
                   filters="list",
                   bunch="Filter"),
         prototype = list(name = "",
                          description = "",
                          filters = list(new("Filter")),
                          bunch = new("Filter"))
)

#Show method
setMethod(f = "show",
          signature = "FilterStack",
          definition = function(object){

            cat("name:", object@name, "\n")
            cat("description:", object@description,"\n")
            cat("number of filters:", length(object@filters),"\n")

            if(length(object@filters)>1){

              filters.name <- object@filters[[1]]@name

              for(i in 2:length(object@filters)){
                temp.name <- object@filters[[i]]@name
                filters.name <- paste(filters.name, temp.name, sep= ", ")
              }
            }else{
              filters.name <- "No filters in the stack."
            }

            cat("filter names:", filters.name, "\n")
            cat("bunch:")
            cat("\t Thickness:", object@bunch@thickness, "[mm] \n")
            cat("\t Reflexion (1-P):", object@bunch@reflexion*100, "[%] \n")
            cat("\t transmission:", "\n")
            cat("\t \t ...from:", min(object@bunch@transmission[,1]), "to", max(object@bunch@transmission[,1]), "[nm]. \n")
            cat("\t \t ...between:", min(object@bunch@transmission[,2])*100, "and", max(object@bunch@transmission[,2])*100, "[%]. \n")
          })

#Set method

## Generic
#' Method setFilterStack
#'
#' @name FilterStack-class
#' @rdname FilterStack-class
#'
#' @param name
#'  \link{character}: Name of the filter stack.
#' @param description
#'  \link{character}: Description of the filter stack.
#' @param filters
#'  \link{list}: List of the \linkS4class{Filter} which are in the filter stack.
#'
#' @exportMethod setFilterStack
#'

setGeneric(name="setFilterStack",
           def=function(name,description,filters){standardGeneric("setFilterStack")}
)


## Method
#' @rdname FilterStack-class
#' @aliases setFilterStack setFilterStack,FilterStack-method

setMethod(f = "setFilterStack",
          signature = c(name="character",
                        description="character",
                        filters=list()),
          definition = function(name,description,filters){

            for(i in 1:length(filters)){
              if(!is(filters[[i]], "Filter")){
                stop("[set_FilterStack] Error: list can only contain Filter object")
              }
            }

            temp.bunch <- filters[[1]]

            if(length(filters)>1){
              temp.description <- paste("Combination of the filters:", filters[[1]]@name)

              for(i in 2: length(filters)){
                temp.bunch <- combine_Filters(temp.bunch, filters[[i]])
                temp.description <- paste(temp.description, "and", filters[[i]]@name)
              }
            }else{
              temp.description <- temp.bunch@description
            }


            new.bunch <- temp.bunch
            new.bunch@description <- temp.description


            new.object <- new("FilterStack")
            new.object@name <- name
            new.object@description <- description
            new.object@filters <- filters
            new.object@bunch <- new.bunch

            return(new.object)
          })

# Get method

## Generic
#' Method getFilterStack
#'
#' @name FilterStack-class
#' @rdname FilterStack-class
#'
#' @param object
#'  \linkS4class{FilterStack}: FilterStack
#' @param ref
#'  \link{character}: FilterStack slot.
#'
#' @exportMethod getFilterStack


setGeneric(name="getFilterStack",
           def=function(object,ref){standardGeneric("getFilterStack")}
)

## Method
#' @rdname FilterStack-class
#' @aliases getFilterStack getFilterStack,Material-method

setMethod(f = "getFilterStack",
          signature = c(object = "FilterStack",
                        ref = "character"),
          definition = function(object, ref){

            if(ref == "name"){
              return(object@name)

            }else if(ref == "description"){
              return(object@description)

            }else if(ref == "filters"){
              return(object@filters)

            }else{
              return(object@bunch)
            }

          })
