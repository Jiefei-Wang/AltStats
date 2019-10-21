context("Test group generic")

A <- runif(10)

lengthFunc <- function(x) {
    length(x)
}
getElementFunc <- function(x, i) {
    x[i]
}

setAltClass(className = "groupGeneric", "double")
setAltMethod(className = "groupGeneric", getLength = lengthFunc)
setAltMethod(className = "groupGeneric", getElement = getElementFunc)


#Create altWrapper object
B <- newAltrep(className = "groupGeneric", x = A)
B_s3 <- newAltrep(className = "groupGeneric", x = A, S3Class = T)
B_s4 <- newAltrep(className = "groupGeneric", x = A, S4Class = T)

test_that("Test add", {
    expect_error(B + 1)
    expect_equivalent(as.numeric(B_s3 + 1), A + 1)
    expect_equivalent(as.numeric(B_s4 + 1), A + 1)
})

test_that("Test minus", {
    expect_error(B - 1)
    expect_equivalent(as.numeric(B_s3 - 1), A - 1)
    expect_equivalent(as.numeric(B_s4 - 1), A - 1)
})
test_that("Test comparison", {
    expect_error(B == A[1])
    expect_equivalent(B_s3 == A[1], A == A[1])
    expect_equivalent(B_s4 == A[1], A == A[1])
})

test_that("Test math", {
    #expect_equal(sqrt(B),sqrt(A))
    #expect_equivalent(B_s3 == A[1], A == A[1])
    #expect_equivalent(B_s4 == A[1], A == A[1])
    
    
    expect_error(cumsum(B))
    expect_equivalent(as.numeric(cumsum(B_s3)), cumsum(A))
    expect_equivalent(as.numeric(cumsum(B_s4)), cumsum(A))
})


