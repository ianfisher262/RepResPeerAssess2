empdata<-data.frame(matrix(NA,nrow=nrow(vocdrpenr),ncol=max(max(vocdrpenr$drpmon,vocdrpenr$vocmon))))
empnam<-paste0("mon",1:ncol(empdata))
colnames(empdata)<-empnam

for (i in 1:nrow(empdata)) {
  #go by month
  for (j in 1:ncol(empdata)) {
    empdata[i,j]<-ifelse(j<vocdrpenr[i,7],0,
                         (ifelse(j>=vocdrpenr[i,7] & j<vocdrpenr[i,3] & i<vocdrpenr[i,5],1,
                                 (ifelse(j>=vocdrpenr[i,3] & j<vocdrpenr[i,5],2,3)))))
  }
}

dave<-cbind(vocdrpenr,empdata)