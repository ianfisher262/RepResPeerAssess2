##read in data
summ_scc<-readRDS("C:/Users/q791225/Documents/My SAS Files(32)/r/data/summarySCC_PM25.rds")
scc<-readRDS("C:/Users/q791225/Documents/My SAS Files(32)/r/data/Source_Classification_Code.rds")

##q1 - Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##need to find total emissions in tons for all pollutants then plot

library(plyr)
summ<-ddply(summ_scc,"year",summarise,total_emissions=sum(Emissions))
summ$emm_plot<-summ$total_emissions/1000000

numyear<-length(summ$year)

library(RColorBrewer)

mycol<-brewer.pal(numyear, "Dark2")
ylim_val0<-min(floor(summ$emm_plot))
ylim_val1<-max(ceiling(summ$emm_plot))

png("plot1.png")

barplot(summ$emm_plot, 
        main="Total PM2.5 emissions in USA from 1999 to 2008", 
        sub="Total emissions have decreased from 1999 to 2008",
        xlab="Year",
        ylab="Total emissions (Millions of tons)",
        col=mycol,
        ylim=c(0,ylim_val1),
        names.arg=summ$year,
        border=NA)

dev.off()
