devtools::load_all()
A <- NA_integer_

lengthFunc <- function(x) {
    length(x)
}
getElementFunc <- function(x, i) {
    x[i]
}

setAltClass(className = "testClass", "integer")
setAltMethod(className = "testClass", getLength = lengthFunc)
setAltMethod(className = "testClass", getElement = getElementFunc)


B <- newAltrep(className = "testClass", x = A, S4Class = T)

B
B & FALSE



B + 1L
B == NA_integer_
cumsum(B)
range(B)

sqrt(B)
