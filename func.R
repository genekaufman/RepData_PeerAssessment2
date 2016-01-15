replaceMultiplier <- function(x){
#  print(paste("2 x:",x))
  ret<-0
  x_num <- suppressWarnings(as.numeric(x))
#  print(paste("5 x_num:",x_num))
  if (is.numeric(x_num) && !(is.na(x_num))) {
    ret<-10 ** x_num
#    print(paste("8 ret:",ret))
  } else if(x=="H") {
    ret<-100
  } else if(x=="K") {
    ret<-1000
  } else if(x=="M") {
    ret<-1000000
  } else if(x=="B") {
    ret<-1000000000
#    print(paste("17 ret:",ret))

  } else { # anything else is garbage, set to zero so that calculation is zero
    ret<-0
  }
  ret<-paste(x," -> ",ret)
  print(ret)
}

mytest<-c("","?","-","+","0","2","H","K","M","B","Z","B")
mydf <- data.frame(mytest)
names(mydf)<-c("myvar")

#print(replaceMultiplier("B"))
mydf$newvar<-
  sapply(as.character(mydf$myvar), FUN=replaceMultiplier)

cleanAllButDR <- function(){
  rm(list=ls()[ls()!="data_raw" & ls()!="cleacleanAllButDR"])
}