getAltClass<-function(type, S3Class = FALSE){
    if(S3Class){
        return(classNamesInherit[[type]])
    }
    ind<-which(classNames$type==type)
    if(length(ind)==1){
        return(classNames$name[ind])
    }
    stop("Cannot find the corresponding S4 class name")
}


passToDefault <- function(e1, e2) {
    if(is(e1,"altRaw")||(!missing(e2)&&is(e2,"altRaw"))){
        return(TRUE)
    }
    if (dispatchToDefault &&
        C_has_pointer(e1) &&
        (missing(e2) || C_has_pointer(e2))) {
        return(TRUE)
    } else{
        return(FALSE)
    }
}

classInherit <- function(result, resAttr, e1){
    if (is.null(resAttr))
        return(result)
    if(!C_has_pointer(result)){
        if (isS4(e1) && !isS4(result)) {
            result <- newAltWrapper(result, S4Class = TRUE)
        }else{
            result <- newAltWrapper(result, S3Class = TRUE)
        }
        resAttr$class <- class(result)
    }else{
        ## If the result has a data pointer
        ## and its class is an alt class
        ## Remove the class attributes
        if(!is.null(resAttr$class)&&
           any(resAttr$class%in%classNameList)){
            resAttr$class = resAttr$class[!resAttr$class%in%classNameList]
            if(!length(resAttr$class)){
                resAttr$class= NULL
            }
        }else{
            ## If the result is not of an alt class
            ## and it should be a S4 object
            if(!is.null(resAttr$class) && isS4(e1) && !isS4(result)){
                result <- newAltWrapper(result, S4Class = TRUE)
            }
        }
    }
    attributes(result) <- resAttr
    result
}

copyAttributes <- function(result, e1, e2 = NULL){
    if(typeof(e1) != typeof(result)){
        if(!is.null(e2) && typeof(e2) == typeof(result)){
            e1 <- e2
            e2 <- NULL
        }else{
            return(result) 
        }
    }else{
        if(typeof(e2)!=typeof(result)){
            e2 <- NULL
        }
    }
    
    ##Unary operator
    if (is.null(e2)) {
        resAttr <- attributes(e1)
        result <- classInherit(result, resAttr, e1)
        return(result)
    }
    
    ## arguments are not of the same length
    if (length(e1) != length(e2)) {
        if (length(e1) > length(e2)) {
            longerArg <- e1
            shorterArg <- e2
        } else{
            longerArg <- e2
            shorterArg <- e1
        }
        if(typeof(longerArg)!=typeof(result))
            return(result)
        resAttr <- attributes(longerArg)
        result <- classInherit(result, resAttr, longerArg)
        return(result)
    }
    ## arguments have the same length
    ## The attributes in e1 will take precedence over e2 
    e1Attr <- attributes(e1)
    e2Attr <- attributes(e2)
    if(is.null(e1Attr)&&is.null(e2Attr))
        return(result)
    if(is.null(e1Attr))
        e1Attr <- list()
    if(is.null(e2Attr))
        e2Attr <- list()
    attrNames <- unique(c(names(e1Attr),names(e2Attr)))
    resAttr <- vector("list",length(attrNames))
    names(resAttr) <- attrNames
    resAttr[names(e1Attr)] <- e1Attr
    e2NewAttr <- attrNames[!attrNames%in%names(e1Attr)]
    resAttr[e2NewAttr] <- e2Attr[e2NewAttr]
    
    result <- classInherit(result, resAttr, e1)
    result
}


## Dispatch function to process the result and 
## copy attributes
genericDispatch <- function(func, .Generic, e1, ...) {
    args <- list(...)
    result <- func(.Generic, e1, ...)
    copyAttributes(result, e1, args$e2)
}