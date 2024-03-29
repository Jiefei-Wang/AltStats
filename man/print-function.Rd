% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classMethodDefine.R
\name{altWrapperPrint}
\alias{altWrapperPrint}
\alias{print.altWrapper}
\alias{printAltWrapper}
\title{Print altWrapper vector values}
\usage{
\method{print}{altWrapper}(x, ...)

printAltWrapper(x, ...)
}
\arguments{
\item{x}{An altWrapper object}

\item{...}{No effect, for compatibility only}
}
\value{
The argument \code{x}
}
\description{
This function is a complement of the print function.
It is able to print ALTREP objects. In case that the
data pointer is not available, the function will use \code{GET_REGION} API to
access the data.
}
\examples{
A <- AltStats:::makeExampleAltrep(runif(4))

tryCatch(print(A), error=function(e) print(e))
tryCatch(printAltWrapper(A), error=function(e) print(e))

}
