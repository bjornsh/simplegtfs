% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spelling.R
\name{spelling}
\alias{spelling}
\title{Correct spelling}
\usage{
spelling(gtfs_obj)
}
\arguments{
\item{gtfs_obj}{GTFS object loaded with tidytransit::read_gtfs()}
}
\description{
Fix spelling of Swedish special letters in case UTF-8 encoding does not work
}
\examples{
\donttest{
library(stringr)
gtfs_file <- list.files("gtfs_data")
dat = tidytransit::read_gtfs(paste0("gtfs_data/", gtfs_file[1]))
funk_spelling(dat)
}

}
