
library(data.table)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(graphics)
library(lubridate)
library(tidyr)

global_superstore_data<- read.csv("Global Superstore.csv",stringsAsFactors = FALSE)
head(global_superstore_data)
str(global_superstore_data)
# Checking missing value
sapply(global_superstore_data, function(x) sum(is.na(x))/nrow(global_superstore_data)) # No missing values
#Almost 81 percent of postal codes are NA and rest of the data don't contain NA
#Removing postal code column
global_superstore_data$Postal.Code<-NULL

#Checking duplicity
unique(global_superstore_data) #Returning 51290 rows, there are no duplicate records

#Data preparation

#Listing the unique market geographically
unique(global_superstore_data$Market)
market_segment<-unique(global_superstore_data[,"Market"])
market_segment
#Listing different product segments
unique(global_superstore_data$Segment)
product_segment<-unique(global_superstore_data[,"Segment"])
product_segment
#Listing 21 different buckets
buckets<-unique(global_superstore_data[,c("Market","Segment")])
buckets

#Extracting Order month from the order date
global_superstore_data$Order.Date<-as.Date(global_superstore_data$Order.Date,"%d-%m-%Y")
str(global_superstore_data)
global_superstore_data$order_month<-month(global_superstore_data$Order.Date)
global_superstore_data$order_month<-month.abb[global_superstore_data$order_month]



#Aggregating profit on the basis of 21 buckets
aggregated_profit<-global_superstore_data %>% select(Market,Segment,Profit,order_month) %>% group_by(Market,Segment,order_month) %>% mutate(monthly_profit= sum(Profit))
aggregated_profit<-aggregated_profit %>% select(Market,Segment,order_month,monthly_profit)
aggregated_profit<-unique(aggregated_profit)

#Plot of monthly profit on the basis of market and segment
aggregated_profit$order_month<-factor(aggregated_profit$order_month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

plot_profit<-ggplot(aggregated_profit,aes(x=order_month,y=monthly_profit,group=1))
plot_profit+geom_point()+geom_line()+facet_grid(Market~Segment)

#Splitting agrregated profit dataframe into 21 different dataframes list on the basis of market and segment
#Using split function
?split
class(aggregated_profit)
aggregated_profit_buckets_new<-aggregated_profit %>% group_by(Market,Segment) %>% summarise(mean_cff=mean(monthly_profit),sd_cff=sd(monthly_profit))
aggregated_profit_buckets_new$coeff_var<-aggregated_profit_buckets_new$sd_cff/aggregated_profit_buckets_new$mean_cff
aggregated_profit_buckets_new<-aggregated_profit_buckets_new %>% arrange(coeff_var)



#What are the 2 most profitable segments ?
#The most profitable buckets are the ones which has the lowest coefficient of variation
aggregated_profit_buckets_new[1:2,]

#Hence we will choose the following segments for further Analysis
buckets_analyze<-data.frame(Market=aggregated_profit_buckets_new[1:2,"Market"],Segment=aggregated_profit_buckets_new[1:2,"Segment"])
buckets_analyze
#below are the most profitable and consistent segment
# Market  Segment
# 1     EU Consumer
# 2   APAC Consumer
#-----------------End of Profit Aggregation------------------------------------------------------------------------------------------------------------------------------------

#-----------------Sales Aggregation-----------------------------------------------------------------------------------

#Lets us get the aggregated Sales for each month for each market and each segment
aggregated_sales<-global_superstore_data %>%select(Market,Segment,Sales,order_month) %>%group_by(Market,Segment,order_month) %>% mutate(monthly_sales=sum(Sales))
aggregated_sales<-aggregated_sales%>% select(Market,Segment,order_month,monthly_sales)
aggregated_sales<-unique(aggregated_sales)
nrow(aggregated_sales)
aggregated_sales


#Aggregated Yearly Sales ( To identify yearly trends)

#Let us plot the aggregated Sales for each Market and each Segment
plot_sales<-ggplot(aggregated_sales,aes(x=order_month,y=monthly_sales,group=1))
plot_sales+geom_point()+geom_line()+facet_grid(Market~Segment)

#Let us get the aggregated Quantity for each month for each market and each segment

aggregated_quantity<-global_superstore_data %>%select(Market,Segment,Quantity,order_month) %>%group_by(Market,Segment,order_month) %>% mutate(monthly_quantity=sum(Quantity))
aggregated_quantity<-aggregated_quantity%>% select(Market,Segment,order_month,monthly_quantity)
aggregated_quantity<-unique(aggregated_quantity)
nrow(aggregated_quantity)
class(aggregated_quantity)

ggplot(aggregated_quantity,aes(x=order_month,y=monthly_quantity,group=1))+geom_point()+geom_line()+facet_grid(Market~Segment)

#Lets merge aggregated Profit,Sales and Quantity and plot the time series
aggregated_data_analyze<-merge(aggregated_profit,aggregated_sales,by=c("Market","Segment","order_month"),all=T)
aggregated_data_analyze<-merge(aggregated_data_analyze,aggregated_quantity,by=c("Market","Segment","order_month"),all=T)
aggregated_data_analyze

#Lets us normalise Monthly Sales,Profit and Quantity
aggregated_data_analyze$monthly_profit<-scale(aggregated_data_analyze$monthly_profit)
aggregated_data_analyze$monthly_sales<-scale(aggregated_data_analyze$monthly_sales)
aggregated_data_analyze$monthly_quantity<-scale(aggregated_data_analyze$monthly_quantity)

plot_all<-ggplot(aggregated_data_analyze,aes(order_month,group=1))+geom_point(aes(y=monthly_profit))+geom_line(aes(y=monthly_profit,colour="monthly_profit"))
plot_all<-plot_all+geom_point(aes(y=monthly_sales))+geom_line(aes(y=monthly_sales,colour="monthly_sales"))+facet_grid(Market~Segment)
plot_all<-plot_all+geom_point(aes(y=monthly_quantity))+geom_line(aes(y=monthly_quantity,colour="monthly_quantity"))
plot_all<-plot_all+scale_color_manual(values=c("red", "blue", "green"))
plot_all


#Let us only select data for the 2 buckets identified from the data

subset_analyze<-filter(global_superstore_data,Market %in% buckets_analyze$Market & Segment %in% buckets_analyze$Segment)

subset_analyze<-subset_analyze %>% select(Market,Segment,Order.Date,order_month,Sales,Quantity)

str(subset_analyze)

#-----------------------END OF DATA PREPARATION------------------------------------------------------------


#Lets Extract Year from Order Date
subset_analyze$order_year<-year(subset_analyze$Order.Date)

#Lets get the monthly sales for each of the 2 segments for each month of each year
aggregated_subset_sales<-subset_analyze %>%select(Market,Segment,Sales,order_month,order_year,Order.Date) %>%group_by(Market,Segment,order_month,order_year) %>% mutate(monthly_sales=sum(Sales))
aggregated_subset_sales

aggregated_subset_quantity<-subset_analyze %>%select(Market,Segment,Quantity,order_month,order_year,Order.Date) %>%group_by(Market,Segment,order_month,order_year) %>% mutate(monthly_demand=sum(Quantity))
aggregated_subset_quantity

#Now for each segment, apply time series

?cut
#--------------TIME SERIES ANALYSIS MODELLING-----------------------------------------------------


smoothen<-function(timeser,w=3){
  
  smoothedseries <- stats::filter(timeser, 
                           filter=rep(1/(2*w+1),(2*w+1)), 
                           method='convolution', sides=2)
  
  #Smoothing left end of the time series
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothing right end of the time series
  
  n <- length(timeser)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  smoothedseries
}

plotExponential<-function(timeser,alphas,title=""){
  plot(timeser,main=title)
  cols<-c('red', 'blue', 'green', 'black')
  
  labels <- c(paste('alpha =', alphas), 'Original')
  for(i in seq(1 , length(alphas))){
    smoothedseries<-HoltWinters(timeser,alpha = alphas[i],beta = F,gamma = F)
    lines(fitted(smoothedseries)[,1],col=cols[i],lwd=2)
    
  }
  legend("topleft", labels,col=cols, lwd=2,pt.cex = 1,cex=0.5)
}


#-----------------------_END OF FUNCTIONS------------------------------------------------------

#---------------------EU Consumer Segment Time Series Analysis---------------------------------------------

#####################Creation of time series data########################
EU_Consumer_Sales<-aggregated_subset_sales %>% filter(Market=="EU" & Segment=="Consumer") %>%  select(Order.Date,monthly_sales)  
EU_Consumer_Sales$Month<-as.Date(cut(EU_Consumer_Sales$Order.Date,breaks="month"))
EU_Consumer_Sales<-EU_Consumer_Sales[,c("Month","monthly_sales")]
EU_Consumer_Sales<- unique(EU_Consumer_Sales)
EU_Consumer_Sales<-arrange(EU_Consumer_Sales,Month)
month=as.numeric(row.names(EU_Consumer_Sales))
EU_Consumer_Sales$Month<-NULL
EU_Consumer_Sales$Month<-month

EU_Consumer_Demand<-aggregated_subset_quantity %>% filter(Market=="EU" & Segment=="Consumer") %>%  select(Order.Date,monthly_demand)  
EU_Consumer_Demand$Month<-as.Date(cut(EU_Consumer_Demand$Order.Date,breaks="month"))
EU_Consumer_Demand<-EU_Consumer_Demand[,c("Month","monthly_demand")]
EU_Consumer_Demand<- unique(EU_Consumer_Demand)
EU_Consumer_Demand<-arrange(EU_Consumer_Demand,Month)
month=as.numeric(row.names(EU_Consumer_Demand))
EU_Consumer_Demand$Month<-NULL
EU_Consumer_Demand$Month<-month

APAC_Consumer_Sales<-aggregated_subset_sales %>% filter(Market=="APAC" & Segment=="Consumer") %>%  select(Order.Date,monthly_sales)  
APAC_Consumer_Sales$Month<-as.Date(cut(APAC_Consumer_Sales$Order.Date,breaks="month"))
APAC_Consumer_Sales<-APAC_Consumer_Sales[,c("Month","monthly_sales")]
APAC_Consumer_Sales<- unique(APAC_Consumer_Sales)
APAC_Consumer_Sales<-arrange(APAC_Consumer_Sales,Month)
month=as.numeric(row.names(APAC_Consumer_Sales))
APAC_Consumer_Sales$Month<-NULL
APAC_Consumer_Sales$Month<-month

APAC_Consumer_Demand<-aggregated_subset_quantity %>% filter(Market=="APAC" & Segment=="Consumer") %>%  select(Order.Date,monthly_demand)  
APAC_Consumer_Demand$Month<-as.Date(cut(APAC_Consumer_Demand$Order.Date,breaks="month"))
APAC_Consumer_Demand<-APAC_Consumer_Demand[,c("Month","monthly_demand")]
APAC_Consumer_Demand<- unique(APAC_Consumer_Demand)
APAC_Consumer_Demand<-arrange(APAC_Consumer_Demand,Month)
month=as.numeric(row.names(APAC_Consumer_Demand))
APAC_Consumer_Demand$Month<-NULL
APAC_Consumer_Demand$Month<-month
##################End of Creation of time series data#####################


#####################Building time series for forecasting EU consumer sales data################## 
EU_Consumer_Sales_timeser_total<-ts(EU_Consumer_Sales$monthly_sales)

plot(EU_Consumer_Sales_timeser_total,col="red",main="EU Consumer Sales Time Series")
EU_Consumer_Sales_42 <- EU_Consumer_Sales[1:42,]
timeser_1 <- ts(EU_Consumer_Sales_42$monthly_sales)
plot(timeser_1)
kpss.test(timeser_1)### not stationary
timeser_1_smoothedseries<-smoothen(timeser = timeser_1,w=1)
# #Smoothing of the timeseries
# plotExponential(timeser=timeser_1,alphas = c(0.2, 0.4, 0.8))
# ##Alpha 0.4 seems to be good
# timeser_1_smoothedseries <- HoltWinters(timeser_1, alpha=0.4,
#                               beta=FALSE, gamma=FALSE)
# lines(fitted(timeser_1_smoothedseries)[,1], col="red", lwd=2)
EU_Consumer_Sales_42_in <- EU_Consumer_Sales_42$Month
lines(timeser_1_smoothedseries, col="blue", lwd=2)


smootheddf1 <- as.data.frame(cbind(EU_Consumer_Sales_42_in, as.vector(timeser_1_smoothedseries)))
colnames(smootheddf1) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_1 <- lm(Sales ~ sin(Month) * poly(Month,3), data=smootheddf1)
global_pred_1 <- predict(lmfit_1, Month=EU_Consumer_Sales_42_in)
summary(global_pred_1)
lines(EU_Consumer_Sales_42_in, global_pred_1, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_1 <- timeser_1-global_pred_1
plot(local_pred_1, col='red', type = "l")
acf(local_pred_1)
acf(local_pred_1, type="partial")
armafit_1 <- auto.arima(local_pred_1)

tsdiag(armafit_1)
armafit_1

#We'll check if the residual series is white noise

resi_1 <- local_pred_1-fitted(armafit_1)
plot(resi_1)
kpss.test(resi_1)
#Both testshows that the series is now pure stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_1 <- EU_Consumer_Sales[43:48,]
outdata_1<-data.frame(outdata_1)
class(outdata_1)
timevals_out_1 <- outdata_1$Month

global_pred_out_1 <- predict(lmfit_1,data.frame(Month =timevals_out_1))

fcast_1 <- global_pred_out_1
class(fcast_1)
#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_1 <- accuracy(fcast_1,outdata_1[,1])[5]
MAPE_class_dec_1 ##28.04 Good low Mape value

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_1 <- c(ts(global_pred_1),ts(global_pred_out_1))
plot(EU_Consumer_Sales_timeser_total, col = "black")
lines(class_dec_pred_1, col = "red")


###############Building time series for forecasting EU consumer demand data ##############
EU_Consumer_Demand_timeser_total<-ts(EU_Consumer_Demand$monthly_demand)

plot(EU_Consumer_Demand_timeser_total,col="red",main="EU Consumer Demand Time Series")
EU_Consumer_Demand_42 <- EU_Consumer_Demand[1:42,]
timeser_2 <- ts(EU_Consumer_Demand_42$monthly_demand)
plot(timeser_2)
kpss.test(timeser_2)### not stationary
timeser_2_smoothedseries<-smoothen(timeser = timeser_2,w=1)
# #Smoothing of the timeseries
# plotExponential(timeser=timeser_1,alphas = c(0.2, 0.4, 0.8))
# ##Alpha 0.4 seems to be good
# timeser_1_smoothedseries <- HoltWinters(timeser_1, alpha=0.4,
#                               beta=FALSE, gamma=FALSE)
# lines(fitted(timeser_1_smoothedseries)[,1], col="red", lwd=2)
EU_Consumer_Demand_42_in <- EU_Consumer_Demand_42$Month
lines(timeser_2_smoothedseries, col="blue", lwd=2)


smootheddf2 <- as.data.frame(cbind(EU_Consumer_Demand_42_in, as.vector(timeser_2_smoothedseries)))
colnames(smootheddf2) <- c('Month', 'Demand')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_2 <- lm(Demand ~ sin(Month) * poly(Month,3), data=smootheddf2)
global_pred_2 <- predict(lmfit_2, Month=EU_Consumer_Demand_42_in)
summary(global_pred_2)
lines(EU_Consumer_Demand_42_in, global_pred_2, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_2 <- timeser_2-global_pred_2
plot(local_pred_2, col='red', type = "l")
acf(local_pred_2)
acf(local_pred_2, type="partial")
armafit_2 <- auto.arima(local_pred_2)

tsdiag(armafit_2)
armafit_2

#We'll check if the residual series is white noise

resi_2 <- local_pred_2-fitted(armafit_2)
plot(resi_2)
kpss.test(resi_2)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_2 <- EU_Consumer_Demand[43:48,]
outdata_2<-data.frame(outdata_2)
timevals_out_2 <- outdata_2$Month

global_pred_out_2 <- predict(lmfit_2,data.frame(Month =timevals_out_2))

fcast_2 <- global_pred_out_2

#Now, let's compare our prediction with the actual values, using MAPE
?accuracy
MAPE_class_dec_2 <- accuracy(fcast_2,outdata_2[,1])[5]
MAPE_class_dec_2 ##35.31684

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_2 <- c(ts(global_pred_2),ts(global_pred_out_2))
plot(EU_Consumer_Demand_timeser_total, col = "black")
lines(class_dec_pred_2, col = "red")


########Building time series for forecasting APAC consumer sales data########### #
APAC_Consumer_Sale_timeser_total<-ts(APAC_Consumer_Sales$monthly_sales)

plot(APAC_Consumer_Sale_timeser_total,col="red",main="APAC Consumer Sales Time Series")
APAC_Consumer_Saless_42 <- APAC_Consumer_Sales[1:42,]
timeser_3 <- ts(APAC_Consumer_Saless_42$monthly_sales)
plot(timeser_3)
kpss.test(timeser_3)### not stationary
timeser_3_smoothedseries<-smoothen(timeser = timeser_3,w=1)
# #Smoothing of the timeseries
# plotExponential(timeser=timeser_1,alphas = c(0.2, 0.4, 0.8))
# ##Alpha 0.4 seems to be good
# timeser_1_smoothedseries <- HoltWinters(timeser_1, alpha=0.4,
#                               beta=FALSE, gamma=FALSE)
# lines(fitted(timeser_1_smoothedseries)[,1], col="red", lwd=2)
APAC_Consumer_Sales_42_in <- APAC_Consumer_Saless_42$Month
lines(timeser_3_smoothedseries, col="blue", lwd=2)


smootheddf3 <- as.data.frame(cbind(APAC_Consumer_Sales_42_in, as.vector(timeser_3_smoothedseries)))
colnames(smootheddf3) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_3 <- lm(Sales ~ sin(Month) * poly(Month,3), data=smootheddf3)
global_pred_3 <- predict(lmfit_3, Month=APAC_Consumer_Sales_42_in)
summary(global_pred_3)
lines(APAC_Consumer_Sales_42_in, global_pred_3, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_3 <- timeser_3-global_pred_3
plot(local_pred_3, col='red', type = "l")
acf(local_pred_3)
acf(local_pred_3, type="partial")
armafit_3 <- auto.arima(local_pred_3)

tsdiag(armafit_3)
armafit_3

#We'll check if the residual series is white noise

resi_3 <- local_pred_3-fitted(armafit_3)
plot(resi_3)
kpss.test(resi_3)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_3 <- APAC_Consumer_Sales[43:48,]
outdata_3<-data.frame(outdata_3)
timevals_out_3 <- outdata_3$Month

global_pred_out_3 <- predict(lmfit_3,data.frame(Month =timevals_out_3))

fcast_3 <- global_pred_out_3
#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_3 <- accuracy(fcast_3,outdata_3[,1])[5]
MAPE_class_dec_3 ##31.15703

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_3 <- c(ts(global_pred_3),ts(global_pred_out_3))
plot(APAC_Consumer_Sale_timeser_total, col = "black")
lines(class_dec_pred_3, col = "red")


############Building time series for forecasting APAC consumer demand data########## 
APAC_Consumer_Demand_timeser_total<-ts(APAC_Consumer_Demand$monthly_demand)

plot(APAC_Consumer_Demand_timeser_total,col="red",main="Apac Consumer Demand Time Series")
APAC_Consumer_Demand_42 <- APAC_Consumer_Demand[1:42,]
timeser_4 <- ts(APAC_Consumer_Demand_42$monthly_demand)
plot(timeser_4)
kpss.test(timeser_4)### not stationary
timeser_4_smoothedseries<-smoothen(timeser = timeser_4,w=1)
# #Smoothing of the timeseries
# plotExponential(timeser=timeser_1,alphas = c(0.2, 0.4, 0.8))
# ##Alpha 0.4 seems to be good
# timeser_1_smoothedseries <- HoltWinters(timeser_1, alpha=0.4,
#                               beta=FALSE, gamma=FALSE)
# lines(fitted(timeser_1_smoothedseries)[,1], col="red", lwd=2)
APAC_Consumer_Demand_42_in <- APAC_Consumer_Demand_42$Month
lines(timeser_4_smoothedseries, col="blue", lwd=2)


smootheddf4 <- as.data.frame(cbind(APAC_Consumer_Demand_42_in, as.vector(timeser_4_smoothedseries)))
colnames(smootheddf4) <- c('Month', 'Demand')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_4 <- lm(Demand ~ sin(Month) * poly(Month,3), data=smootheddf4)
global_pred_4 <- predict(lmfit_4, Month=APAC_Consumer_Demand_42_in)
summary(global_pred_4)
lines(APAC_Consumer_Demand_42_in, global_pred_4, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_4 <- timeser_4-global_pred_4
plot(local_pred_4, col='red', type = "l")
acf(local_pred_4)
acf(local_pred_4, type="partial")
armafit_4 <- auto.arima(local_pred_4)

tsdiag(armafit_4)
armafit_4

#We'll check if the residual series is white noise

resi_4 <- local_pred_4-fitted(armafit_4)
plot(resi_4)
kpss.test(resi_4)


#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata_4 <- APAC_Consumer_Demand[43:48,]
outdata_4<-data.frame(outdata_4)
timevals_out_4 <- outdata_4$Month

global_pred_out_4 <- predict(lmfit_4,data.frame(Month =timevals_out_4))

fcast_4 <- global_pred_out_4
#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec_4 <- accuracy(fcast_4,outdata_4[,1])[5]
MAPE_class_dec_4 ##41.49113

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_4 <- c(ts(global_pred_4),ts(global_pred_out_4))
plot(APAC_Consumer_Demand_timeser_total, col = "black")
lines(class_dec_pred_4, col = "red")

