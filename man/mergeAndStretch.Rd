% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extremes-data.R
\name{mergeAndStretch}
\alias{mergeAndStretch}
\title{Merge and even out two timeseries}
\usage{
mergeAndStretch(points, related)
}
\arguments{
\item{points}{the primary data series containing time and value}

\item{related}{the related data series containing time and value}
}
\value{
updated related list extended to contain NA where gaps exist
}
\description{
merges two timeseries; giving smaller dataset NA values
where a time series gap occurs between them
}
