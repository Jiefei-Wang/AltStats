################################
##Class definition
################################
## Make S3 class compatible with S4 dispatching
setOldClass(c("altRaw","altWrapper"))
setOldClass(c("altLogical","altWrapper"))
setOldClass(c("altInteger","altNumeric","altWrapper"))
setOldClass(c("altDouble","altNumeric","altWrapper"))

#' The S3/S4 classes for ALTREP of type `raw`, `logical`, `integer` and `double`
#'
#' The S3/S4 classes for `raw`, `logical`, `integer` and `double` are 
#' `altRaw`, `altLogical`, `altInteger` and `altDouble` respectively.
#' `altInteger` and `altDouble` inherit `altNumeric` class. 
#' `altRaw`, `altLogical` and `altNumeric` inherit `altWrapper` class.
#' `altWrapper` is a subclass of `ANY` type.
#'
#' @name altWrapperClass
#' @aliases altWrapper-class altNumeric-class altRaw-class 
#' altLogical-class altInteger-class altDouble-class
#' @rdname altWrapperClass
#' @export
setClass(Class = "altWrapper",
         contains = "ANY")
#' @rdname altWrapperClass
#' @export
setClass(Class = "altNumeric",
         contains = "altWrapper")
#' @rdname altWrapperClass
#' @export
setClass(Class = "altRaw",
         contains = "altWrapper")
#' @rdname altWrapperClass
#' @export
setClass(Class = "altLogical",
         contains = "altWrapper")
#' @rdname altWrapperClass
#' @export
setClass(Class = "altInteger",
         contains = "altNumeric")
#' @rdname altWrapperClass
#' @export
setClass(Class = "altDouble",
         contains = "altNumeric")

################################
##Convenient class unions
################################
setClassUnion("baseNumeric", c("logical", "numeric"))
setClassUnion("baseNumericOrMissing", c("baseNumeric", "missing"))
setClassUnion("altNumericUnion", c("altLogical", "altNumeric"))

