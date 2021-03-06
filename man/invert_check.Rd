% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{invert_check}
\alias{invert_check}
\title{Check Invertibility Conditions}
\usage{
invert_check(x)
}
\arguments{
\item{x}{A \code{cx_vec} that has a 1 appended before the coefficents. (e.g. c(1, x))}
}
\value{
True (if outside unit circle) || False (if inside unit circle)
}
\description{
Checks the invertiveness of series of coefficients.
}
\keyword{internal}
