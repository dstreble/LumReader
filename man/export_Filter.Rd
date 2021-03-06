% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_Filter.R
\name{export_Filter}
\alias{export_Filter}
\title{Function to export a filter}
\usage{
export_Filter(object, file.name)
}
\arguments{
\item{object}{\code{\linkS4class{Filter}} to export}

\item{file.name}{\link{character} name of the .FLT file that will contain the filter properties.}
}
\value{
The function creates a new txt file containing the filter properties.
}
\description{
This function exports a filter as a .FLT file.
}
\examples{
example <- default_Filters('example')[[1]]
file.name <- 'example'

\dontrun{
export_Filter(example, file.name)  # uncomment
}

# There is now an 'example.FLT' file in the 'working directory'.
# This file is a classical .txt file despite the extension.

file <- paste(getwd(),'/', file.name,'.FLT', sep="")
print(file)
# readLines(file)


}
\author{
David Strebler, University of Cologne (Germany).
}

