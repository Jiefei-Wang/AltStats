################################
##Class definition
################################
classNames<-data.frame(
    name=c("altRaw", "altLogical", "altInteger", "altDouble"),
    type=c("raw","logical","integer","double"),
    stringsAsFactors = FALSE
)

classNamesInherit <- list(
    raw = c("altRaw","altWrapper"),
    logical = c("altLogical","altWrapper"),
    integer = c("altInteger","altNumeric","altWrapper"),
    double = c("altDouble","altNumeric","altWrapper")
)

classNameList <- unique(unlist(classNamesInherit))

## Make S3 class compatible with S4 dispatching
# setOldClass(c("altRaw","altWrapper"))
# setOldClass(c("altLogical","altWrapper"))
# setOldClass(c("altInteger","altNumeric","altWrapper"))
# setOldClass(c("altDouble","altNumeric","altWrapper"))
for(i in classNamesInherit){
    setOldClass(i)
}


#' The S3/S4 classes for ALTREP of type `raw`, `logical`, `integer` and `double`
#'
#' The S3/S4 classes for `raw`, `logical`, `integer` and `double` are 
#' `altRaw`, `altLogical`, `altInteger` and `altDouble` respectively.
#' `altInteger` and `altDouble` inherit `altNumeric` class. 
#' `altRaw`, `altLogical` and `altNumeric` inherit `altWrapper` class.
#' `altWrapper` is a subclass of `ANY` type.
#'
#' @name altWrapperClass
#' @aliases altWrapper-class altNumeric-class altRaw-class 
#' altLogical-class altInteger-class altDouble-class
#' @rdname altWrapperClass
#' @export
.altWrapper <- setClass(Class = "altWrapper",
         contains = "ANY")
#' @rdname altWrapperClass
#' @export
.altNumeric <- setClass(Class = "altNumeric",
         contains = "altWrapper")
#' @rdname altWrapperClass
#' @export
.altRaw <- setClass(Class = "altRaw",
         contains = "altWrapper")
#' @rdname altWrapperClass
#' @export
.altLogical <- setClass(Class = "altLogical",
         contains = "altWrapper")
#' @rdname altWrapperClass
#' @export
.altInteger <- setClass(Class = "altInteger",
         contains = "altNumeric")
#' @rdname altWrapperClass
#' @export
.altDouble <- setClass(Class = "altDouble",
         contains = "altNumeric")

################################
##Convenient class unions
################################
setClassUnion("baseNumeric", c("logical", "numeric"))
setClassUnion("baseNumericOrMissing", c("baseNumeric", "missing"))
setClassUnion("altNumericUnion", c("altLogical", "altNumeric"))


################################
##Constructor function
################################
#' Make an ALTREP S3 or S4 class
#' 
#' This function will create a wrapper for an ALTREP object.
#' Most functions in `?groupGeneric` have been overrided and support
#' ALTREP object. 
#' 
#' @param x an ALTREP object.
#' @param S3Class logical, whether the result is an S3 object
#' @param S4Class logical, whether the result is an S4 object, if both
#' S3Class and S4Class are TRUE, the package will use S4Class 
#' @param forceSet logical, force R to set the class attribute for the variable x, it
#' violates R's copy-on-change philosophy and cause the variable x 
#' outside of the function also changes. It is useful only when the normal way to create 
#' the class object does not work. The argument only works when `S3Class = TRUE`
#' @examples
#' A <- newAltWrapper(runif(10), S3Class = TRUE)
#' A
#' @return an S3/S4 object
#' @export
newAltWrapper <- function(x, S3Class = TRUE, S4Class, forceSet = FALSE){
    if(missing(S4Class))
        S4Class <- !S3Class
    if(!S3Class&& !S4Class){
        stop("S3 and S4 class cannot be both FALSE")
    }
    
    if(S4Class){
        return(new(getAltClass(typeof(x)), x))
    }
    if(S3Class){
        if(forceSet){
            C_force_attribute_set(x, as.symbol("class"), 
                                  getAltClass(typeof(x), S3Class = TRUE))
        }else{
            class(x) <- getAltClass(typeof(x), S3Class = TRUE)
        }
        
        return(x)
    }
    
}
