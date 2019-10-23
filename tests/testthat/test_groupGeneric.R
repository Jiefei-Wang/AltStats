context("Test group generic")
##Turn off default dispatch

for (debug in c(TRUE, FALSE)) {
    AltStat:::debug(debug)
    
    A <- runif(10) - 0.5
    #Create altWrapper object
    A_s3 <- newAltWrapper(A, S3Class = T)
    A_s4 <- newAltWrapper(A, S4Class = T)
    
    test_that("Test add", {
        expect_equivalent(as.numeric(A_s3 + 1), A + 1)
        expect_equivalent(as.numeric(A_s4 + 1), A + 1)
    })
    
    test_that("Test minus", {
        expect_equivalent(as.numeric(A_s3 - 1), A - 1)
        expect_equivalent(as.numeric(A_s4 - 1), A - 1)
    })
    test_that("Test comparison", {
        expect_equivalent(A_s3 == A[2], A == A[2])
        expect_equivalent(A_s4 == A[2], A == A[2])
    })
    
    test_that("Test math", {
        expect_equivalent(as.numeric(cumsum(A_s3)), cumsum(A))
        expect_equivalent(as.numeric(cumsum(A_s4)), cumsum(A))
        
        
        expect_equivalent(as.numeric(ceiling(A_s3)), ceiling(A))
        expect_equivalent(as.numeric(ceiling(A_s4)), ceiling(A))
        
        
        expect_equivalent(as.numeric(sign(A_s3)), sign(A))
        expect_equivalent(as.numeric(sign(A_s4)), sign(A))
    })
    test_that("Test range", {
        tmp <- A
        tmp1 <- A_s3
        tmp2 <- A_s4
        expect_equivalent(range(A_s3), range(A))
        A[1] <- NA
        A_s3[1] <- NA
        expect_equivalent(range(A_s3), range(A))
        expect_equivalent(range(A_s3, na.rm = TRUE), range(A, na.rm = TRUE))
        expect_equivalent(range(A_s3, finite = TRUE), range(A, finite = TRUE))
        A[2] <- Inf
        A_s3[2] <- Inf
        expect_equivalent(range(A_s3), range(A))
        expect_equivalent(range(A_s3, na.rm = TRUE), range(A, na.rm = TRUE))
        expect_equivalent(range(A_s3, finite = TRUE), range(A, finite = TRUE))
        A_s3 <- tmp1
        A <- tmp
        
        expect_equivalent(range(A_s4), range(A))
        A[1] <- NA
        A_s4[1] <- NA
        expect_equivalent(range(A_s4), range(A))
        expect_equivalent(range(A_s4, na.rm = TRUE), range(A, na.rm = TRUE))
        expect_equivalent(range(A_s4, finite = TRUE), range(A, finite = TRUE))
        A[2] <- Inf
        A_s4[2] <- Inf
        expect_equivalent(range(A_s4), range(A))
        expect_equivalent(range(A_s4, na.rm = TRUE), range(A, na.rm = TRUE))
        expect_equivalent(range(A_s4, finite = TRUE), range(A, finite = TRUE))
        A_s4 <- tmp2
        A <- tmp
    })
    
    
}