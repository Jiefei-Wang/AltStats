S4ClassNames<-data.frame(
    name=c("altRaw", "altLogical", "altInteger", "altReal"),
    type=c("raw","logical","integer","double"),
    stringsAsFactors = FALSE
)


getAltS4Class<-function(type){
    ind<-which(S4ClassNames$type==type)
    if(length(ind)==1){
        S4ClassNames$name[ind]
    }
    stop("Cannot find the corresponding S4 class name")
}