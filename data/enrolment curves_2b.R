library(xlsx)
library(reshape)
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)

enr1<-read.xlsx("u:/enrolment curve.xlsx","Sheet1")

##turn blanks to zeros
enr1[is.na(enr1)]<-0

#calculate patients coming in each month
enr1$difenrb<-ifelse(enr1$Best==0,0,c(0,diff(enr1$Best,lag=1)))
enr1$difenrm<-ifelse(enr1$Best==0,0,c(0,diff(enr1$Median,lag=1)))
enr1$difenrw<-ifelse(enr1$Best==0,0,c(0,diff(enr1$Worst,lag=1)))

##turn months into dates
protdat<-as.Date("2016-06-01")
enr1$enrmon<-as.Date(protdat+(enr1$Month*round((365.25/12))))

##get max date of study e.g. 2w run-in, 12w trt, 3w f/u
enr1$lasttrt<-enr1$enrmon+(7*17)

enr1a<-enr1[,c(1,3)]

patno<-1:max(enr1a$Median)

enr1b<-merge(patno,enr1a,all=TRUE)

enr1b1<-enr1b[which(enr1b$x<=enr1b$Median),]

enr1b1<-arrange(enr1b1,x,Month,Median)

enrsum<-ddply(enr1b1,"x",summarise,minmon=min(Month))
enrsum<-rename(enrsum,c("x"="Patient","minmon"="Month"))

firstmon<-min(enrsum$Month)+1

############################################################################
#calculating events
############################################################################

for (j in 1:1000) {
  
  nam<-paste("voc",j,sep="")
  
    for (i in firstmon:1000) {
      
      #take only patients who could be available - only need patient number
      tempdf<-as.data.frame(enrsum[which(enrsum$Month<=i),])
      
      #if patient had voc then remove
      
      if (i>firstmon) {
        if (length(voc$Patient)>0) {
          tempdf<-tempdf[which(!(tempdf$Patient %in% voc$Patient)),]
        }
      }
      
      #count number of patients in dataset
      numpat<-length(tempdf$Patient)
      
      #test for VOC
      tempdf$voc<-ifelse(runif(n=numpat,min=0,max=1)>0.93,1,0)
      
      #create datasets for patients with and without voc
      tempvoc<-tempdf[which(tempdf$voc==1),]
      
      #add on month where the patient's voc is simulated to occur
      if (length(tempvoc$voc>0)) {tempvoc$Month<-i}
      
      #if first pass then create voc dataset, else rbind this months data to the existing voc dataset
      if (i==firstmon) {
        voc<-tempvoc
      } else {
        if (length(tempvoc$voc>0)) {
          voc<-rbind(voc,tempvoc)
      }
      }
      rm(list=c("tempvoc","tempdf","numpat"))
      if (length(voc$Patient)==400) {break      }
    }
  voc<-select(arrange(voc,Patient))
  assign(nam,voc)
}

dave<-do.call(cbind.data.frame, mget(paste0('voc', 1:1000)))

#remove individual simulation datasets
to.remove <- ls()
to.remove <- c(to.remove[grepl("voc", to.remove)], "to.remove")
rm(list=to.remove)

#select only first patient variable but keep all months of voc
voc<-dave[,c("voc1.Patient",grep("Month", names(dave), value=TRUE))]
rm(dave)

vocmelt<-melt(voc,id="voc1.Patient")

#calculate percentiles
vocsum<-ddply(vocmelt,
              "voc1.Patient",
              summarise,
              p25=round(quantile(value,0.25)),
              p50=round(quantile(value,0.50)),
              p75=round(quantile(value,0.75)))

vocsum<-rename(vocsum, c("voc1.Patient"="Patient"))

vocsummelt<-melt(vocsum,id="Patient")
vocsummelt$voc<-1

##count patients each month that have a voc
inext<-ddply(vocsummelt,c("variable","value"),summarize,inext=sum(voc))
inext<-rename(inext, c("variable"="Percentile","value"="Month"))

#take max months from voc - this is as long as the study could possibly last
maxmon<-max(inext$Month)

#get a dataset of only months where patients have been enrolled
enr1out<-enr1a[which(enr1a$Median>0 & enr1a$Month>=firstmon),]

#get last month of enrolment +1
maxenrmon<-max(enr1out$Month)+1

#get max patients
totpat<-max(enr1$Median)

#################################################################################
# drop out
#################################################################################

#calculate the probability of a patient remaining in the study if we assume
#a 5% per quarter drop out rate
#using formula converting rate to probability of :
#p=1-exp(-rt)
#
#where:
#  
#  p=probability;
#  r= instantaneous rate, provided that it is constant over the period of
#     interest (t)
drpprob<-exp(-(0.05/3))

for (k in 1:1000) {
  
  nam<-paste("drp",k,sep="")
  
  for (i in firstmon:1000) {
    
    #take only patients who could be available - only need patient number
    tempdf<-as.data.frame(enrsum[which(enrsum$Month<=i),])
    
    #if patient had drp then remove
    
    if (i>firstmon) {
      if (length(drp$Patient)>0) {
        tempdf<-tempdf[which(!(tempdf$Patient %in% drp$Patient)),]
      }
    }
    
    #count number of patients in dataset
    numpat<-length(tempdf$Patient)
    
    #test for drp
    tempdf$drp<-ifelse(runif(n=numpat,min=0,max=1)>drpprob,1,0)
    
    #create datasets for patients with and without drp
    tempdrp<-tempdf[which(tempdf$drp==1),]
    
    #add on month where the patient's drp is simulated to occur
    if (length(tempdrp$drp>0)) {tempdrp$Month<-i}
    
    #if first pass then create drp dataset, else rbind this month's data to the existing drp dataset
    if (i==firstmon) {
      drp<-tempdrp
    } else {
      if (length(tempdrp$drp>0)) {
        drp<-rbind(drp,tempdrp)
      }
    }
    rm(list=c("tempdrp","tempdf","numpat"))
    if (length(drp$Patient)==400) {break      }
  }
  drp<-select(arrange(drp,Patient))
  assign(nam,drp)
}

dave<-do.call(cbind.data.frame, mget(paste0('drp', 1:1000)))

#remove individual simulation datasets
to.remove <- ls()
to.remove <- c(to.remove[grepl("drp", to.remove)], "to.remove")
rm(list=to.remove)

#select only first patient variable but keep all months of drp
drp<-dave[,c("drp1.Patient",grep("Month", names(dave), value=TRUE))]
rm(dave)

drpmelt<-melt(drp,id="drp1.Patient")

#calculate percentiles
drpsum<-ddply(drpmelt,
              "drp1.Patient",
              summarise,
              p25=round(quantile(value,0.25)),
              p50=round(quantile(value,0.50)),
              p75=round(quantile(value,0.75)))

drpsum<-rename(drpsum, c("drp1.Patient"="Patient"))

drpsummelt<-melt(drpsum,id="Patient")
drpsummelt$drp<-1

##count patients each month that have dropped out
drpout<-ddply(drpsummelt,c("variable","value"),summarize,drpout=sum(drp))
drpout<-rename(drpout, c("variable"="Percentile","value"="Month"))

#####################################################################################
# when each patient was enrolled
#####################################################################################

enr5<-enr1out
enr5$patsin<-lag(enr5$Median)

for (i in min(enr5$Month):max(enr5$Month)) {
  #create names for each set of data
  nam<-paste("monx",i,sep="")
  if (i==min(enr5$Month)) {
    mon<-enr5[which(enr5$Month==i),]
    monrange<-mon$Median
    monrange1<-cbind(i,monrange)
    assign(nam,monrange1)
  } else {
    mon<-enr5[which(enr5$Month==i),]
    monrange<-(mon$patsin+1):mon$Median
    monrange1<-cbind(i,monrange)
    assign(nam,monrange1)
  }
  rm(list=c("monrange","monrange1","mon","nam"))
}

#stack the individual datasets up
dave<-do.call(rbind.data.frame, mget(paste0('monx', min(enr5$Month):max(enr5$Month))))
#rename the variables
monin<-rename(dave,c("i"="Month","monrange"="patin"))
rm(dave)

#remove individual simulation datasets
to.remove <- ls()
to.remove <- c(to.remove[grepl("monx", to.remove)], "to.remove")
rm(list=to.remove)

##########################################################################################
# bring together enrolment, voc and drop out
##########################################################################################

#merge voc and drop out data
vocdrp<-merge(vocsummelt,drpsummelt,by=c("Patient","variable"))

#merge on months enrolled for each patient
vocdrpenr<-merge(vocdrp,enrsum,by="Patient")
vocdrpenr<-rename(vocdrpenr,c("value.x"="vocmon","value.y"="drpmon","Month"="enrmon"))

#want a curve of patients still on study and those on the extension
#create empty dataset for month calculations
#want a dataset with same rows as the combined full dataset with cols to match max
#number of months for either drop out or vocs
empdata<-data.frame(matrix(NA,nrow=nrow(vocdrpenr),ncol=max(max(vocdrpenr$drpmon,vocdrpenr$vocmon))))
empnam<-paste0("mon",1:ncol(empdata))
colnames(empdata)<-empnam

for (i in 1:nrow(empdata)) {
  #go by month
  for (j in 1:ncol(empdata)) {
    empdata[i,j]<-ifelse(j<vocdrpenr[i,7],0,
                         (ifelse(j>=vocdrpenr[i,7] & j<vocdrpenr[i,3] & j<vocdrpenr[i,5],1,
                                 (ifelse(j>=vocdrpenr[i,3] & j<vocdrpenr[i,5],2,3)))))
  }
}

fulldata<-rename(cbind(vocdrpenr,empdata),c("variable"="Percentile"))

fullmelt<-filter(melt(fulldata,id="Percentile"),grepl("^mon",variable)==TRUE)

fullsum<-ddply(fullmelt,c("Percentile","variable","value"),summarize,count=length(value))
fullsum<-arrange(fullsum,Percentile,value,variable)
fullsum<-mutate(fullsum,
                Month=as.numeric(gsub("mon","",variable)),
                grpvar=ifelse(value==0,paste(Percentile,"-","Not enrolled"),
                              (ifelse(value==1,paste(Percentile,"-","Enrolled"),
                                      (ifelse(value==2,paste(Percentile,"-","On extension"),
                                              paste(Percentile,"-","Dropped out"))))))         
                )

ggplot(filter(fullsum,value %in% c(1,2)),aes(x=Month,y=count,group=grpvar,color=grpvar)) +
  geom_line()
  