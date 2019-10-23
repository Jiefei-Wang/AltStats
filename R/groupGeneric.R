passMathToDefault <- function(generic, e1) {
    passToDefault(e1) || generic %in% c("digamma", "trigamma")
}

mathOperator <- function(.Generic, e1) {
    #message("called")
    if(.Generic %in% c("cummax", "cummin", "cumprod", "cumsum")){
        C_math_partial_operator(.Generic, e1)
    }else{
        C_math_operator(.Generic, e1)
    }
}


opsOperator <- function(.Generic, e1, e2) {
    #message("called")
    ## unary operator
    if (missing(e2)) {
        if (length(e1) == 0) {
            return(vector(typeof(e1)))
        } else{
            result <- C_arith_unary_operator(.Generic, e1)
        }
        return(result)
    }
    
    ## Binary operator
    if (length(e1) == 0 || length(e2) == 0) {
        result <- callGeneric(vector(typeof(e1)), vector(typeof(e2)))
    } else{
        result <- C_arith_binary_operator(.Generic, e1,  e2)
    }
    result
}

rangeFunc <- function(args, na.rm = FALSE, finite = FALSE){
    #message("called")
    minMaxResult <- matrix(0L, length(args), 2)
    for (i in seq_along(args)) {
        if(passToDefault(args[[i]])){
            minMaxResult[i, ] <-range(args[[i]])
        }else{
            minMaxResult[i, ] <- C_range_function(args[[i]], na.rm, finite)
        }
    }
    minResult <- minMaxResult[, 1]
    maxResult <- minMaxResult[, 2]
    if(finite){
        minResult=minResult[is.finite(minResult)]
        maxResult=maxResult[is.finite(maxResult)]
    }
    c(min(minResult, na.rm =na.rm), max(maxResult, na.rm =na.rm))
} 

