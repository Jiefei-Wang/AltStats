arith_operator<-function(.Generic,e1,e2){
    if(missing(e2)){
        result = arith_operator_unary(.Generic,e1)
    }
    if(length(e1) == 0||length(e2) == 0){
        result=callGeneric(vector(typeof(e1)),vector(typeof(e2)))
    }else{
        result=C_arith_operator(.Generic,e1,e2)
    }
    
    if(!isS4(result)){
        result=new(getAltS4Class(typeof(result)),result)
    }
    result
}


setMethod("Arith", signature = signature("altLogical","numeric"),
          function(e1,e2){
              arith_operator(.Generic,e1,e2)
          })

setMethod("Arith", signature = signature("altInteger","numeric"),
          function(e1,e2){
              arith_operator(.Generic,e1,e2)
          })

setMethod("Arith", signature = signature("altReal","numeric"),
          function(e1,e2){
              arith_operator(.Generic,e1,e2)
          })

setMethod("Arith", signature = signature("numeric","altReal"),
          function(e1,e2){
              arith_operator(.Generic,e2,e1)
          })



