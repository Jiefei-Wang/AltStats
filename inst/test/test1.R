setClass("test",contains = "integer")
a=new("test",1L:10L)
a

setwd("D:/OneDrive/course material/work/Roswell park/AltStat/AltStat")
devtools::load_all()
C_arith_binary_operator("+",1:10,2)


C_arith_unary_operator("-",as.numeric(1L:555L))
1


a=structure(1:10,class="altInteger")
a
-a



a=structure(1:10,class="altInteger")
Ops.altInteger<-function(e1,e2){
    arith_operator(.Generic,e1,e2)
}

arith_operator<-function(.Generic,e1,e2){
    do.call("NextMethod",list(),envir=parent.frame())
}
-a



a=structure(c(1L:3L, -1L:1L/0L),class=c("altInteger","altNumeric","altWrapper"))

a=structure(c(1,4,2),class="altDouble")
cumsum(a)

cumprod(a)

a=new("altInteger",1:10)


range(a)
range(a, na.rm = TRUE)
range(a, finite = TRUE)
range(a, finite = TRUE, na.rm=TRUE)







