% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{checkParams}
\alias{checkParams}
\title{Check the Parameters}
\usage{
checkParams(params, require.len, default, null.is.fine,
  env = parent.frame())
}
\arguments{
\item{params}{A \code{character vector} that specifies the user supplied parameters.}

\item{require.len}{An \code{integer vector} that specifies the required length of each parameter.}

\item{default}{A \code{list} that specifies the default of each parameter.}

\item{null.is.fine}{A \code{boolean vector} to indicate whether \code{NULL} is fine for parameters.}

\item{env}{An \code{environment} to use.}
}
\description{
Check the user supplied parameters and assign them to the default if they are wrong.
}
\details{
The user supplied parameters are usually \code{line.color}, \code{line.type}, \code{point.size},
\code{point.shape}, \code{CI.color} and \code{legend.label}.

This function will check whether the required length of the parameter is met. If not, it will assign the
default value to that parameter.
}
\keyword{internal}
