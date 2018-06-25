install.packages("stringi")
install.packages("ggplot")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("tidyverse")

library(grid)
library(gridExtra)
library(tidyverse)
library(ggplot2)
library(reshape2)

rm(list = ls())
#reading file into data frame
mydata = read.table ("c:/ECN_QC1.csv", header = TRUE, sep = ",", row.names=NULL)
varName = unique(mydata$FIELDNAME)

######################################################################################
##                          Average by year                                         ##
######################################################################################
#an empty data.frame to store average of the values per year
mydata_avg = data.frame(Ints=integer(),Doubles=double(),Factors=factor())
#loop to calculate the average of each impurity by year and store them in mydata_avg
for(i in 1: length(varName))
{
  onev_mydata = mydata[mydata$FIELDNAME == varName[i],]
  
  onev_myData_meanY = aggregate(VALUE ~ Year+ FIELDNAME, onev_mydata, mean)
    
  mydata_avg = rbind(mydata_avg, onev_myData_meanY)
}
print(mydata_avg)

ggplot(data = mydata_avg) + geom_point(mapping = aes(x = Year, y = VALUE)) + facet_wrap(~ FIELDNAME, nrow = 5)


#Printing each plot one by one and stored them on hard drive

mydata_avg_varSplit = split(mydata_avg, mydata_avg$FIELDNAME)
nn = length(mydata_avg_varSplit)
for(i in 1:nn)
{
  mypath <- paste( varName[i], ".jpg", sep = "")
  mytitle = paste("Title: ", varName[i])
  
  p = ggplot(data = mydata_avg_varSplit[[i]]) + 
    geom_point(mapping = aes(x = Year, y = VALUE)) +
    ggtitle(mydata_avg_varSplit[[i]]$FIELDNAME)

  ggsave(p, filename = mypath)
  
}
ggplot() +geom_line(data = mydata_avg[mydata_avg$VALUE<20,], aes(x = Year, y = VALUE, color = FIELDNAME), size = 1)
######################################################################################
##                          Average by year + Site                                  ##
######################################################################################

mydata_avg_overall = aggregate(VALUE ~ Year+SITECODE+FIELDNAME,mydata, mean)
mydata_site = split(mydata_avg_overall,mydata_avg_overall$SITECODE)
siteName = unique(mydata$SITECODE)
nn = length(mydata_site)
for(i in 1:nn)
{
  myfile <- paste( siteName[i], ".jpg", sep = "")
  p = ggplot() +geom_line(data = mydata_site[[i]], aes(x = Year, y = VALUE, color = FIELDNAME), size = 1)
  ggsave(p,filename = myfile)
  
}
