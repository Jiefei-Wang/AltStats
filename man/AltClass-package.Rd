\name{AltClass-package}
\alias{AltClass-package}
\alias{AltClass}
\docType{package}
\title{
  A Patch for R's Operators to Support ALTREP objects
}
\description{
  The package provides a quick fix for ALTREP. 
    An ALTREP object is an R object that behaves like an R atomic vector (e.g. integer vector). 
    The definition of an ALTREP object is provided by users and it is possible that an ALTREP object
    does not define a data pointer function. In this case, most R functions will throw an error 
    when operate on the ALTREP objects. This package defines S3 and S4 classes to overload common
    R functions to support these operations.
}
\author{
Author: Jiefei Wang
Maintainer: Jiefei Wang <szwjf08@gmail.com>
}
\keyword{ package }
\examples{
  ## Please see the vignitte to find an example
}
