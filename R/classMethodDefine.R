#' @include groupGeneric.R

#' @export
Ops.altNumeric <- function(e1, e2) {
    if (passToDefault(e1, e2)) {
        NextMethod()
    } else{
        genericDispatch(opsOperator, .Generic, e1 = e1, e2 = e2)
    }
}

#' @export
Math.altNumeric <- function(x, ...) {
    if (passMathToDefault(.Generic, x)) {
        NextMethod()
    } else{
        genericDispatch(mathOperator, .Generic, x)
    }
}

#' @export
range.altNumeric <- function(..., na.rm = FALSE, finite = FALSE) {
    args <- list(...)
    argsPassDefault <- all(sapply(args, passToDefault))
    if (argsPassDefault) {
        return(NextMethod())
    }
    rangeFunc(list(...), na.rm, finite)
}



setMethod("range", signature = signature("altNumeric"),function(x, ..., na.rm = FALSE){
    finite <- FALSE
    args <- list(...)
    if(!is.null(args$finite)){
        finite <- args$finite
        args$finite <- NULL
    }
    
    if(length(args)!=0){
        args <- list(args, x)
    }else{
        args <- x
    }
    argsPassDefault <- all(sapply(args, passToDefault))
    if (argsPassDefault) {
        return(callNextMethod())
    }
    rangeFunc(args, na.rm, finite)
})



setMethod("Arith", signature = signature("altNumeric", "baseNumericOrMissing"),
          function(e1, e2) {
              if (passToDefault(e1, e2)) {
                  callNextMethod()
              } else{
                  genericDispatch(opsOperator, .Generic, e1 = e1, e2 = e2)
              }
          })

setMethod("Arith", signature = signature("baseNumeric", "altNumeric"),
          function(e1, e2) {
              if (passToDefault(e1, e2)) {
                  callNextMethod()
              } else{
                  genericDispatch(opsOperator, .Generic, e1 = e2, e2 = e1)
              }
          })
setMethod("Compare", signature = signature("altNumeric", "baseNumericOrMissing"),
          function(e1, e2) {
              if (passToDefault(e1, e2)) {
                  callNextMethod()
              } else{
                  opsOperator(.Generic, e1, e2)
              }
          })

setMethod("Compare", signature = signature("baseNumeric", "altNumeric"),
          function(e1, e2) {
              if (passToDefault(e1, e2)) {
                  callNextMethod()
              } else{
                  opsOperator(.Generic, e1, e2)
              }
          })

setMethod("Math", signature = signature("altNumeric"),
          function(x) {
              if (passMathToDefault(.Generic, x)) {
                  callNextMethod()
              } else{
                  genericDispatch(mathOperator, .Generic, x)
              }
          })

