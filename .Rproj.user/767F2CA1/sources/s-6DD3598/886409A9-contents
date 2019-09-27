dispatchToDefault=TRUE

passToDefault<-function(e1,e2){
    if(dispatchToDefault&&
       C_has_ptr(e1)&&
       (missing(e2)||C_has_ptr(e2))){
        return(TRUE)
    }else{
        return(FALSE)
    }
}

genericDispatch<-function(func,.Generic,e1,...){
    result <- func(.Generic,e1,...)
    if(isS4(e1) && !isS4(result)){
        result <- new(getAltS4Class(typeof(result)),result)
    }
    result
}

opsOperator<-function(.Generic,e1,e2){
    message("called")
    ## unary operator
    if(missing(e2)){
            if(length(e1)==0){
                return(vector(typeof(e1)))
            }else{
                result <- C_arith_unary_operator(.Generic,e1)
            }
        return(result)
    }
    
    ## Binary operator
    if(length(e1) == 0||length(e2) == 0){
        result <- callGeneric(vector(typeof(e1)),vector(typeof(e2)))
    }else{
        result <- C_binary_arith_operator(.Generic,e1,e2)
    }
    result
}



#' @export
Ops.altInteger<-function(e1,e2){
    if(passToDefault(e1,e2)){
        NextMethod()
    }else{
        generic_dispatch(opsOperator,.Generic,e1,e2)
    }
}


setMethod("Arith", signature = signature("altInteger","missing"),
          function(e1,e2){
              if(passToDefault(e1,e2)){
                  callNextMethod()
              }else{
                  generic_dispatch(opsOperator,.Generic,e1,e2)
              }
          })

setMethod("Arith", signature = signature("altLogical","numeric"),
          function(e1,e2){
              if(passToDefault(e1,e2)){
                  callNextMethod()
              }else{
                  generic_dispatch(opsOperator,.Generic,e1,e2)
              }
          })

setMethod("Arith", signature = signature("altInteger","numeric"),
          function(e1,e2){
              if(passToDefault(e1,e2)){
                  callNextMethod()
              }else{
                  generic_dispatch(opsOperator,.Generic,e1,e2)
              }
          })

setMethod("Arith", signature = signature("altReal","numeric"),
          function(e1,e2){
              if(passToDefault(e1,e2)){
                  callNextMethod()
              }else{
                  generic_dispatch(opsOperator,.Generic,e1,e2)
              }
          })

setMethod("Arith", signature = signature("numeric","altReal"),
          function(e1,e2){
              if(passToDefault(e1,e2)){
                  callNextMethod()
              }else{
                  generic_dispatch(opsOperator,.Generic,e1,e2)
              }
          })



