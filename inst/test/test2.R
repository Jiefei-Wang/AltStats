devtools::load_all()

A_compact = list(start = 1L, by = 0L, length = 10L)

#Define ALTREP APIs
lengthFunc <- function(x) {
    return(x$length)
}
getPrt<-function(x,writable){
    if(is.null(x$ptr)){
    message("Creating data")
    x$ptr = seq(from = x$start ,
            to = x$start +x$by*(x$length-1L), 
            length.out = x$length)
    setAltSelfData(x)
    }
    x$ptr
}
getElementFunc <- function(x, i) {
    if(is.null(x$ptr)){
    return(x$start + x$by * (i - 1))
    }else{
        x$ptr[i]
    }
}

#Register ALTREP class and functions
setAltClass(className = "compactSeq", "integer")
setAltMethod(className = "compactSeq", getLength = lengthFunc)
setAltMethod(className = "compactSeq", getElement = getElementFunc)
#setAltMethod(className = "compactSeq", getDataptr = getPrt)

#Create altWrapper object
A = makeAltrep(className = "compactSeq", x = A_compact, S4Class = T)

A
sqrt(A)
A & FALSE


A+1L
A==1L
cumsum(A)
range(A)
















library(IRanges)

ir = IRanges(A_vector,10L)
is.altrep(ir@start)
class(ir@start)



ir2 = shift(ir,1L)
is.altrep(ir2@start)
class(ir@start)
getAltData1(ir2@start)



i2=as(ir,"IPosRanges")

#Function: coerce (package methods)
#from="ANY", to="IPosRanges"
#setAs("ANY", "IPosRanges", function(from) as(from, "IRanges"))


