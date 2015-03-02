corr<-function(directory,treshold=0){
  directory<-as.character(directory)
  treshold<-as.numeric(treshold)
  result <-vector(mode="numeric", length=0)
  for (i in 1:332) {
    if (i < 10){
      z <-paste("00", i, sep="")
    }else if (10 <= i && i < 100){
      z <- paste(0, i, sep ="")
    }else {
      z <- as.character(i)
    }
    data<-read.csv(paste(z,".csv",sep=""))
    good <- complete.cases(data)
    nobs <- nrow(data[good,])
    if (nobs>treshold){
      x<-cor(c(data[,2]),c(data[,3]),use="complete.obs")
      result <- c(result, x)
    }
  }
  result   
}

cr <- corr("specdata", 150)
head(cr)
