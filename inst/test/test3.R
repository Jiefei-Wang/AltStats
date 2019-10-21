devtools::load_all()

A_compact = list(start = 1L, by = 0L, length = 10L)
#Define ALTREP APIs
lengthFunc <- function(x) {
    return(x$length)
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

#Create altWrapper object
A <- newAltrep(className = "compactSeq", x = A_compact, S4Class = T)

A
sqrt(A)
A & FALSE


A + 1L
A == 1L
cumsum(A)
range(A)





