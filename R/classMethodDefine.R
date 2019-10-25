#' @include groupGeneric.R
NULL

#' altWrapper group generic functions
#' 
#' The usage of the group generic functions is the same as R's native functions.
#' Please see R's document for help.
#' 
#' @param e1 left operand
#' @param e2 right operand
#' @param x argument
#' @param na.rm logical, indicating if NA's should be omitted.
#' @param finite logical, indicating if all non-finite elements should be omitted.
#' @param ... additional arguments
#' @aliases range,altWrapper-method
#' @return Please see the document of R's native functions to find the return value
#' @rdname groupGeneric
#' @export
Ops.altWrapper <- function(e1, e2) {
    if (passToDefault(e1, e2)) {
        NextMethod()
    } else{
        genericDispatch(opsOperator, .Generic, e1 = e1, e2 = e2)
    }
}

#' @rdname groupGeneric
#' @export
Math.altWrapper <- function(x, ...) {
    if (passMathToDefault(.Generic, x)) {
        NextMethod()
    } else{
        genericDispatch(mathOperator, .Generic, x)
    }
}

#' @rdname groupGeneric
#' @export
range.altWrapper <- function(..., na.rm = FALSE, finite = FALSE) {
    args <- list(...)
    argsPassDefault <- all(vapply(args, passToDefault,logical(1)))
    if (argsPassDefault) {
        return(NextMethod())
    }
    rangeFunc(list(...), na.rm, finite)
}



setMethod("range", signature = signature("altWrapper"),
          function(x, ..., na.rm = FALSE) {
              finite <- FALSE
              args <- list(...)
              if (!is.null(args$finite)) {
                  finite <- args$finite
                  args$finite <- NULL
              }
              
              if (length(args) != 0) {
                  args <- list(args, x)
              } else{
                  args <- list(x)
              }
              argsPassDefault <- all(vapply(args, passToDefault,logical(1)))
              if (argsPassDefault) {
                  return(callNextMethod())
              }
              rangeFunc(args, na.rm, finite)
          })



setMethod("Arith", signature = signature("altWrapper", "baseNumericOrMissing"),
          function(e1, e2) {
              if (passToDefault(e1, e2)) {
                  callNextMethod()
              } else{
                  genericDispatch(opsOperator, .Generic, e1 = e1, e2 = e2)
              }
          })

setMethod("Arith", signature = signature("baseNumeric", "altWrapper"),
          function(e1, e2) {
              if (passToDefault(e1, e2)) {
                  callNextMethod()
              } else{
                  genericDispatch(opsOperator, .Generic, e1 = e2, e2 = e1)
              }
          })
setMethod("Compare", signature = signature("altWrapper", "baseNumericOrMissing"),
          function(e1, e2) {
              if (passToDefault(e1, e2)) {
                  callNextMethod()
              } else{
                  opsOperator(.Generic, e1, e2)
              }
          })

setMethod("Compare", signature = signature("baseNumeric", "altWrapper"),
          function(e1, e2) {
              if (passToDefault(e1, e2)) {
                  callNextMethod()
              } else{
                  opsOperator(.Generic, e1, e2)
              }
          })

setMethod("Math", signature = signature("altWrapper"),
          function(x) {
              if (passMathToDefault(.Generic, x)) {
                  callNextMethod()
              } else{
                  genericDispatch(mathOperator, .Generic, x)
              }
          })

################################
##print method
################################

#' @title Print altWrapper vector values
#'
#' @description This function is a complement of the print function. 
#' It is able to print ALTREP objects. In case that the
#' data pointer is not available, the function will use `GET_REGION` API to 
#' access the data.
#'
#' @param x An altWrapper object
#' @param ... No effect, for compatibility only
#' @examples
#' A <- AltStats:::makeExampleAltrep(runif(4))
#' 
#' tryCatch(print(A), error=function(e) print(e))
#' tryCatch(printAltWrapper(A), error=function(e) print(e))
#'
#' @return The argument `x`
#' @name altWrapperPrint
#' @rdname print-function
#' @export
print.altWrapper <- function(x, ...) {
    if(C_has_pointer(x))
        return(NextMethod())
    printAltWrapper(x)
    invisible(x)
}

## Set print dispatch for S4 class
setMethod("show", "altWrapper", function(object)
{
    if(C_has_pointer(object))
        return(callNextMethod())
    printAltWrapper(object)
    invisible(object)
})

#' @rdname print-function
#' @export
printAltWrapper <- function(x, ...) {
    ## If x has a pointer, we can pass it to the default method
    if(C_has_pointer(x)){
       return(print(x))
    }
    
    ## Create a temporary variable to print the value of x
    maxPrint <- getOption("max.print")
    printSize <- min(maxPrint, length(x))

    out = vector(typeof(x), printSize)
    attributes(out) = attributes(x)
    C_copy_altrep_value(out, x)
    
    print(out)
    ## Print truncate information
    if (printSize < length(x)) {
        cat(
            '[ reached getOption("max.print") -- omitted ',
            length(x) - printSize,
            ' entries ]'
        )
    }
    invisible(x)
}






