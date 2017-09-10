#Data Sourcing
getwd()
uber<-read.csv("Uber.csv",stringsAsFactors = FALSE)
#If not installed also use install.packages("package name") for each of the below.
library(lubridate)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)



#Data Cleaning
str(uber)
sum(is.na(uber$Request.timestamp))
sum(is.na(uber$Drop.timestamp))
uber$Request.timestamp<-gsub("/","-",uber$Request.timestamp,fixed = TRUE)
uber$Drop.timestamp<-gsub("/","-",uber$Drop.timestamp,fixed = TRUE)
uber$Request.timestamp<-ifelse(nchar(uber$Request.timestamp)<19,paste(uber$Request.timestamp, "00", sep=":"),paste(uber$Request.timestamp,"", sep=""))
uber$Drop.timestamp<-ifelse(nchar(uber$Drop.timestamp)<19,paste(uber$Drop.timestamp, "00", sep=":"),paste(uber$Drop.timestamp,"", sep=""))


#Converting Date Field in standard format
uber$Request.timestamp<-as.POSIXct(uber$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
uber$Drop.timestamp<-as.POSIXct(uber$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")





#Deriving Request Hour Column for creating hour buckets
 uber$request_hour<-format(uber$Request.timestamp, "%H")
 
# uber_airport<-filter(uber,uber$Pickup.point=="Airport")
# uber$request_hour<-as.numeric(uber$request_hour)
# uber_city<-filter(uber,uber$Pickup.point=="City" & uber$request_hour>3 & uber$request_hour<10)

#Finding time difference of Almost same time for one trip on every hour, no finding
uber$timediff<-difftime(uber$Drop.timestamp,uber$Request.timestamp)
uber$timediff<-as.numeric(uber$timediff)
uber_diff_time_group<-uber %>%group_by(request_hour) %>%summarise(count=mean(timediff,na.rm=TRUE))



#Creating the group by request hour, pickup point and Status of the request
#and plotting a frequency polygon for stressing the problem of
#cancellation and non-availablity faced by uber
uber_group<-uber %>%group_by(Pickup.point, request_hour,Status) %>%summarise(count=length(Request.id))
str(uber_group)
uber_group$request_hour<-as.numeric(uber_group$request_hour)
uber_group$Status <- factor(uber_group$Status)
uber_group$Pickup.point <- factor(uber_group$Pickup.point)

p <- ggplot(data = uber_group,aes(x=uber_group$request_hour,col=Status,y=count))
p <- p +geom_freqpoly(stat = "identity")+xlab("Hour Of the Day") +ylab("No Of Requests") 
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Pickup.point, ncol=1)
p



#Grouping and sengmenting for finding the demand supply curve for different 
#time buckets
uber_group_grouping<-uber_group %>%group_by(request_hour,Pickup.point) %>%summarise(total=sum(count))
uber_final<-merge(uber_group,uber_group_grouping,by= c("request_hour","Pickup.point"))
str(uber_final)
uber_trip_completed<-filter(uber_final,uber_final$Status=="Trip Completed")
str(uber_trip_completed)
uber_trip_completed$request_hour<-as.numeric(uber_trip_completed$request_hour)
uber_trip_completed$gap_percent<-(uber_trip_completed$total-uber_trip_completed$count)/uber_trip_completed$total*100
#Univariate Analysis on the derived Gap Percent Column
mean(uber_trip_completed$gap_percent)


str(uber_group)
str(uber_trip_completed)
uber_trip_completed$gap_percent<-as.integer(uber_trip_completed$gap_percent)
uber_trip_completed_city<-filter(uber_trip_completed,Pickup.point=="City")
uber_trip_completed_airport<-filter(uber_trip_completed,Pickup.point=="Airport")
p1 <- ggplot(data = uber_trip_completed,aes(x=request_hour,y=gap_percent))
p1 <- p1 +geom_line(stat = "identity")+geom_hline(yintercept = 47.6218)+annotate("text", x=1, y=53, label = "Average")
p1 <- p1 + scale_fill_brewer(palette="Set3")
p1 <- p1 + facet_wrap( ~ Pickup.point, ncol=1)
p1
