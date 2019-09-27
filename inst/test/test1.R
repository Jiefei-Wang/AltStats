setClass("test",contains = "integer")
a=new("test",1L:10L)
a
setMethod("Arith", signature = signature("altInteger"),
          function(e1,e2){
              arith_operator(.Generic,e1,e2)
          })
a+1

Sys.getpid()

setwd("D:/OneDrive/course material/work/Roswell park/AltStat/AltStat")
devtools::load_all()
C_binary_arith_operator("+",1:2,2)


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

