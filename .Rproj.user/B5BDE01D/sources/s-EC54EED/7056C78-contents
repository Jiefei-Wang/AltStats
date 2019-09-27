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
#setAltMethod(className = "compactSeq", getDataptr = getPrt)
setAltMethod(className = "compactSeq", getElement = getElementFunc)

#Create altWrapper object
A_vector = makeAltrep(className = "compactSeq", x = A_compact, S4Class=T)

A_vector


B = A_vector+1L
getAltData1(A_vector)


setClass("compactSeq",contains = "altInteger")
setMethod("Arith", signature = signature("compactSeq","numeric"),
          function(e1,e2){
              if(length(e2)==1&&is.integer(e2)){
                  altData=getAltData1(e1)
                  if(is.null(altData$ptr)){
                      e1=duplicateObject(e1,FALSE)
                      altData$start=altData$start+e2
                      setAltData1(e1,altData)
                      return(e1)
                  }
              }
              e1+e2
          })

A_compact = new("compactSeq",A_vector)

C = A_compact+1L


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

