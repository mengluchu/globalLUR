% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeraster2file.R
\name{mergeraster2file}
\alias{mergeraster2file}
\title{Merge new predictor variables (rasters)}
\usage{
mergeraster2file(tomerge, rasterfile, varname)
}
\arguments{
\item{tomerge}{the dataframe to merge the raster files to}

\item{rasterfile}{the raster file to merge}

\item{varname}{the name of the new variable}
}
\value{
a dataframe with merged variables
}
\description{
Merge new predictor variables (rasters)
}
\examples{
merged = mergeraster2file(merged, 'C:/Users/Lu000012/Documents/files/GLOBAL_LUR/predictor variables/elevation.map', 'elevation')
}
