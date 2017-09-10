#With cylinder binning and no derived metrics
library("MASS")
library("car")
library(stringr)
carpricing<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)
#Data Cleaning Code
str(carpricing)

sum(is.na(carpricing))
#NO NA values
carpricing$car_ID<-NULL#For checking unique and car_ID won't be any helpful in modelling
unique(carpricing)

#No duplicate rows as unique return 205 observations
#Checking Outliers
#wheelbase
quantile(carpricing$wheelbase,seq(0,1,0.01))
x<-carpricing$wheelbase
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$wheelbase[which(carpricing$wheelbase>outlier_value)]<-outlier_value

# carlength
quantile(carpricing$carlength,seq(0,1,0.01))
x<-carpricing$carlength
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5* IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$carlength[which(carpricing$carlength>outlier_value)]<-outlier_value
carpricing$carlength[which(carpricing$carlength<156.060)]<-156.060

#carwidth
quantile(carpricing$carwidth,seq(0,1,0.01))
x<-carpricing$carwidth
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$carwidth[which(carpricing$carwidth>outlier_value)]<-outlier_value

# carheight
quantile(carpricing$carheight,seq(0,1,0.01))
x<-carpricing$carheight
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H

#curbweight
quantile(carpricing$curbweight,seq(0,1,0.01))
x<-carpricing$curbweight
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$curbweight[which(carpricing$curbweight>outlier_value)]<-outlier_value
carpricing$curbweight[which(carpricing$curbweight<inlier_value)]<-inlier_value

#enginesize
quantile(carpricing$enginesize,seq(0,1,0.01))
x<-carpricing$enginesize
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$enginesize[which(carpricing$enginesize>outlier_value)]<-outlier_value
carpricing$enginesize[which(carpricing$enginesize<inlier_value)]<-inlier_value

#boreration
quantile(carpricing$boreratio,seq(0,1,0.01))
x<-carpricing$boreratio
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$boreratio[which(carpricing$boreratio>outlier_value)]<-outlier_value
carpricing$boreratio[which(carpricing$boreratio<2.9100)]<-2.9100

#stroke
quantile(carpricing$stroke,seq(0,1,0.01))
x<-carpricing$stroke
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$stroke[which(carpricing$stroke>outlier_value)]<-outlier_value
carpricing$stroke[which(carpricing$stroke<inlier_value)]<-inlier_value


#compressionratio
quantile(carpricing$compressionratio,seq(0,1,0.01))
x<-carpricing$compressionratio
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$compressionratio[which(carpricing$compressionratio>outlier_value)]<-outlier_value

#horsepower
quantile(carpricing$horsepower,seq(0,1,0.01))
x<-carpricing$horsepower
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$horsepower[which(carpricing$horsepower>outlier_value)]<-outlier_value
carpricing$horsepower[which(carpricing$horsepower<inlier_value)]<-inlier_value

#peakrpm
quantile(carpricing$peakrpm,seq(0,1,0.01))
x<-carpricing$peakrpm
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$peakrpm[which(carpricing$peakrpm>outlier_value)]<-outlier_value
carpricing$peakrpm[which(carpricing$peakrpm<inlier_value)]<-inlier_value

#citympg
quantile(carpricing$citympg,seq(0,1,0.01))
x<-carpricing$citympg
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$citympg[which(carpricing$citympg>outlier_value)]<-outlier_value
carpricing$citympg[which(carpricing$citympg<inlier_value)]<-inlier_value

#highwaympg
quantile(carpricing$highwaympg,seq(0,1,0.01))
x<-carpricing$highwaympg
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
inlier_value<-qnt[2]-H
carpricing$highwaympg[which(carpricing$highwaympg>outlier_value)]<-outlier_value
carpricing$highwaympg[which(carpricing$highwaympg<inlier_value)]<-inlier_value



#Converting categorical variables to factor variables using as.factor and making dummy variables out of them.
carpricing$symboling<-as.factor(carpricing$symboling)
summary(carpricing$symboling)
#Creating new levels for symboling as [-2:1] as safe,[0,1] as neutral and [2,3] as risky
levels(carpricing$symboling)[1:2]<-"safe"
levels(carpricing$symboling)[2:3]<-"neutral"
levels(carpricing$symboling)[3:4]<-"risky"
dummy_symboling<-model.matrix(~carpricing$symboling-1,data = carpricing)
dummy_symboling<-dummy_symboling[,-1]
carpricing<-cbind(carpricing[,-1],dummy_symboling)
str(carpricing)


#Converting fuel type to numeric
carpricing$fueltype<-as.factor(carpricing$fueltype)
summary(carpricing$fueltype)
levels(carpricing$fueltype)<-c(1,0) # or c(0,1)(0-gas,1-diesel)
carpricing$fueltype<- as.numeric(levels(carpricing$fueltype))[carpricing$fueltype]


#Converting aspiration to nummeric
carpricing$aspiration<-as.factor(carpricing$aspiration)
summary(carpricing$aspiration)
levels(carpricing$aspiration)<-c(1,0) # or c(0,1)(0-turbo,1-std)
carpricing$aspiration<- as.numeric(levels(carpricing$aspiration))[carpricing$aspiration]


#Converting door number to numeric
carpricing$doornumber<-as.factor(carpricing$doornumber)
summary(carpricing$doornumber)
levels(carpricing$doornumber)<-c(1,0) # or c(0,1)(0-two,1-four)
carpricing$doornumber<- as.numeric(levels(carpricing$doornumber))[carpricing$doornumber]


#Dummy variables for drive wheel
carpricing$drivewheel<-as.factor(carpricing$drivewheel)
summary(carpricing$drivewheel)
dummy_drivewheel<-model.matrix(~carpricing$drivewheel-1,data = carpricing)
dummy_drivewheel<-dummy_drivewheel[,-1]
carpricing<-cbind(carpricing[,-6],dummy_drivewheel)
str(carpricing)

#Converting engine location to numeric
carpricing$enginelocation<-as.factor(carpricing$enginelocation)
summary(carpricing$enginelocation)
levels(carpricing$enginelocation)<-c(1,0) # or c(0,1)(0-rear,1-front)
carpricing$enginelocation<- as.numeric(levels(carpricing$enginelocation))[carpricing$enginelocation]

#Dummy variables for cylinder number
carpricing$cylindernumber<-as.factor(carpricing$cylindernumber)
summary(carpricing$cylindernumber)
levels(carpricing$cylindernumber)
levels(carpricing$cylindernumber)[c(3,2,4)]
levels(carpricing$cylindernumber)[c(1,6)]<-"high"
levels(carpricing$cylindernumber)[c(3,2,4)]<-"medium"
levels(carpricing$cylindernumber)[3:4]<-"low"

dummy_cylindernumber<-model.matrix(~carpricing$cylindernumber-1,data = carpricing)
dummy_cylindernumber<-dummy_cylindernumber[,-1]
carpricing<-cbind(carpricing[,-13],dummy_cylindernumber)
str(carpricing)

#Extracting carnames
carpricing$CarBrand<-unlist(lapply(carpricing$CarName,function (x) strsplit(x," ",fixed = TRUE)[[1]][1]))
carpricing$CarBrand<-tolower(carpricing$CarBrand)



#Data Quality issues in car brand
# 1. vokswagen,volkswagen,vw are same
# 2. toyota,toyouta are same
# 3. porcshce,porsche are same
# 4. mazda and maxda are same
str(carpricing)
carpricing[which(carpricing$CarBrand=="vw"),29]<-"volkswagen"
carpricing[which(carpricing$CarBrand=="vokswagen"),29]<-"volkswagen"
carpricing[which(carpricing$CarBrand=="toyouta"),29]<-"toyota"
carpricing[which(carpricing$CarBrand=="porcshce"),29]<-"porsche"
carpricing[which(carpricing$CarBrand=="maxda"),29]<-"mazda"

#Dummy Variables for Car Brand
carpricing$CarBrand<-as.factor(carpricing$CarBrand)
summary(carpricing$CarBrand)
#Dummy variables for car brand
dummy_CarBrand<-model.matrix(~carpricing$CarBrand-1,data = carpricing)
dummy_CarBrand<-dummy_CarBrand[,-1]
carpricing<-cbind(carpricing[,-29],dummy_CarBrand)
carpricing$CarName<-NULL
str(carpricing)

#Dummy variables for car carbody
carpricing$carbody<-as.factor(carpricing$carbody)
summary(carpricing$carbody)
dummy_carbody<-model.matrix(~carpricing$carbody-1,data = carpricing)
dummy_carbody<-dummy_carbody[,-1]
carpricing<-cbind(carpricing[,-4],dummy_carbody)

#Dummy variables for car enginetype
carpricing$enginetype<-as.factor(carpricing$enginetype)
summary(carpricing$enginetype)
dummy_enginetype<-model.matrix(~carpricing$enginetype-1,data = carpricing)
dummy_enginetype<-dummy_enginetype[,-1]
carpricing<-cbind(carpricing[,-10],dummy_enginetype)

#Dummy variables for car fuelsystem
carpricing$fuelsystem<-as.factor(carpricing$fuelsystem)
summary(carpricing$fuelsystem)
dummy_fuelsystem<-model.matrix(~carpricing$fuelsystem-1,data = carpricing)
dummy_fuelsystem<-dummy_fuelsystem[,-1]
carpricing<-cbind(carpricing[,-11],dummy_fuelsystem)

#Derived Metrics was used for an experiment but was useful in the final model so not creating
# 1. carvolume= carheight*carwidth*carlength
# 2. curbweight/carvolume ratio
# 3. horsepower/enginesize ratio
# carpricing$volume<-carpricing$carwidth*carpricing$carheight*carpricing$carlength
# carpricing$cardensity<-carpricing$curbweight/carpricing$volume
# carpricing$hpratioengsize<-carpricing$horsepower/carpricing$enginesize
#Model Creation

# Divide into training and test data set
#set the seed to 100
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carpricing), 0.7*nrow(carpricing))
# generate the train data set
carpricing_train = carpricing[trainindices,]

#Similarly store the rest of the observations into an object "test".
carpricing_test = carpricing[-trainindices,]


str(carpricing_train)
#Dependent Variable is price
model_1 <-lm(price~.,data=carpricing_train)
summary(model_1)

#Following stepAIC method
#Strategy Followed for removing variables.
###1. Remove variables with high VIF and low significance
###2. Check the correlation for high VIF and high significance Varibale and
## remove it using USE AND TRIAL METHOD
###3. Preference given to high significane
###4. Once the VIF is under 2 for all variables then the variable with p value less then 0.05 are kept
###5 Model creation stopped once all variables are significant and vif value less then 2


step <- stepAIC(model_1, direction="both")
step
model_2<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              carwidth + curbweight + enginesize + stroke + compressionratio + 
              citympg + `carpricing$symbolingneutral` + `carpricing$symbolingrisky` + 
              `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
              `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
              `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
              `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
              `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
              `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
              `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
              `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
              `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
              `carpricing$CarBrandvolkswagen` + `carpricing$CarBrandvolvo` + 
              `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
              `carpricing$carbodysedan` + `carpricing$carbodywagon` + `carpricing$enginetypeohc` + 
              `carpricing$fuelsystem2bbl` + `carpricing$fuelsystemmpfi`, 
            data = carpricing_train)
summary(model_2)
vif(model_2)

#removing carwidth

model_3<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              curbweight + enginesize + stroke + compressionratio + 
              citympg + `carpricing$symbolingneutral` + `carpricing$symbolingrisky` + 
              `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
              `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
              `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
              `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
              `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
              `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
              `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
              `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
              `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
              `carpricing$CarBrandvolkswagen` + `carpricing$CarBrandvolvo` + 
              `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
              `carpricing$carbodysedan` + `carpricing$carbodywagon` + `carpricing$enginetypeohc` + 
              `carpricing$fuelsystem2bbl` + `carpricing$fuelsystemmpfi`, 
            data = carpricing_train)
summary(model_3)
vif(model_3)

#removing fuelsystemmpfi

model_4<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              curbweight + enginesize + stroke + compressionratio + 
              citympg + `carpricing$symbolingneutral` + `carpricing$symbolingrisky` + 
              `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
              `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
              `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
              `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
              `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
              `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
              `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
              `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
              `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
              `carpricing$CarBrandvolkswagen` + `carpricing$CarBrandvolvo` + 
              `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
              `carpricing$carbodysedan` + `carpricing$carbodywagon` + `carpricing$enginetypeohc` + 
              `carpricing$fuelsystem2bbl` , 
            data = carpricing_train)
summary(model_4)
vif(model_4)

#removing fuelsystem2bbl

model_5<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              curbweight + enginesize + stroke + compressionratio + 
              citympg + `carpricing$symbolingneutral` + `carpricing$symbolingrisky` + 
              `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
              `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
              `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
              `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
              `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
              `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
              `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
              `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
              `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
              `carpricing$CarBrandvolkswagen` + `carpricing$CarBrandvolvo` + 
              `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
              `carpricing$carbodysedan` + `carpricing$carbodywagon` + `carpricing$enginetypeohc` , 
            data = carpricing_train)
summary(model_5)
vif(model_5)


#removing  citympg

model_6<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              curbweight + enginesize + stroke + compressionratio + 
               `carpricing$symbolingneutral` + `carpricing$symbolingrisky` + 
              `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
              `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
              `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
              `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
              `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
              `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
              `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
              `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
              `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
              `carpricing$CarBrandvolkswagen` + `carpricing$CarBrandvolvo` + 
              `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
              `carpricing$carbodysedan` + `carpricing$carbodywagon` + `carpricing$enginetypeohc` , 
            data = carpricing_train)
summary(model_6)
vif(model_6)


#removing carbodysedan

model_7<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              curbweight + enginesize + stroke + compressionratio + 
              `carpricing$symbolingneutral` + `carpricing$symbolingrisky` + 
              `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
              `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
              `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
              `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
              `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
              `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
              `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
              `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
              `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
              `carpricing$CarBrandvolkswagen` + `carpricing$CarBrandvolvo` + 
              `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
               `carpricing$carbodywagon` + `carpricing$enginetypeohc` , 
            data = carpricing_train)
summary(model_7)
vif(model_7)


#removing symbolingneutral

model_8<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              curbweight + enginesize + stroke + compressionratio + 
               `carpricing$symbolingrisky` + 
              `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
              `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
              `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
              `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
              `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
              `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
              `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
              `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
              `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
              `carpricing$CarBrandvolkswagen` + `carpricing$CarBrandvolvo` + 
              `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
              `carpricing$carbodywagon` + `carpricing$enginetypeohc` , 
            data = carpricing_train)
summary(model_8)
vif(model_8)


#removing enginetypeohc

model_9<-lm(formula = price ~ aspiration + enginelocation + wheelbase + 
              curbweight + enginesize + stroke + compressionratio + 
              `carpricing$symbolingrisky` + 
              `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
              `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
              `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
              `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
              `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
              `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
              `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
              `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
              `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
              `carpricing$CarBrandvolkswagen` + `carpricing$CarBrandvolvo` + 
              `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
              `carpricing$carbodywagon` , 
            data = carpricing_train)
summary(model_9)
vif(model_9)


#removing wheelbase

model_10<-lm(formula = price ~ aspiration + enginelocation + 
               curbweight + enginesize + stroke + compressionratio + 
               `carpricing$symbolingrisky` + 
               `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$CarBrandvolkswagen` + `carpricing$CarBrandvolvo` + 
               `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_10)
vif(model_10)

#removing symbolingrisky

model_11<-lm(formula = price ~ aspiration + enginelocation + 
               curbweight + enginesize + stroke + compressionratio + 
               `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$CarBrandvolkswagen` + `carpricing$CarBrandvolvo` + 
               `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_11)
vif(model_11)

#removing CarBrandvolvo`

model_12<-lm(formula = price ~ aspiration + enginelocation + 
               curbweight + enginesize + stroke + compressionratio + 
               `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$CarBrandvolkswagen` + 
               `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_12)
vif(model_12)

#removing CarBrandvolkswagen

model_13<-lm(formula = price ~ aspiration + enginelocation + 
               curbweight + enginesize + stroke + compressionratio + 
               `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_13)
vif(model_13)


#removing aspiration

model_14<-lm(formula = price ~  enginelocation + 
               curbweight + enginesize + stroke + compressionratio + 
               `carpricing$drivewheelfwd` + `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_14)
vif(model_14)

#removing drivewheelfwd

model_15<-lm(formula = price ~  enginelocation + 
               curbweight + enginesize + stroke + compressionratio + 
                `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_15)
vif(model_15)

cor(carpricing_train$enginesize,carpricing_train$curbweight)
#Hit and trial for removing either of curbweight and enginesize
#removing enginesize

model_16<-lm(formula = price ~  enginelocation + 
               curbweight  + stroke + compressionratio + 
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_16)
vif(model_16)


#removing stroke

model_17<-lm(formula = price ~  enginelocation + 
               curbweight  +  compressionratio + 
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_17)
vif(model_17)


#removing compressionratio

model_18<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$carbodyhardtop` + `carpricing$carbodyhatchback` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_18)
vif(model_18)

#removing carbodyhatchback

model_19<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$carbodyhardtop` +  
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_19)
vif(model_19)

#removing carbodyhardtop

model_20<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` + `carpricing$CarBrandsaab` + 
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_20)
vif(model_20)


#removing CarBrandsaab

model_21<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` +  
               `carpricing$CarBrandsubaru` + `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_21)
vif(model_21)


#removing CarBrandsubaru

model_22<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandrenault` +  
                `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_22)
vif(model_22)


#removing CarBrandrenault

model_23<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` + `carpricing$CarBrandplymouth` + 
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_23)
vif(model_23)


#removing CarBrandplymouth

model_24<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + `carpricing$CarBrandnissan` + 
               `carpricing$CarBrandpeugeot` +  
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_24)
vif(model_24)

#removing CarBrandnissan

model_25<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` + `carpricing$CarBrandmercury` + 
               `carpricing$CarBrandmitsubishi` + 
               `carpricing$CarBrandpeugeot` +  
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_25)
vif(model_25)


#removing CarBrandmercury
model_26<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBranddodge` + `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` +  
               `carpricing$CarBrandmitsubishi` + 
               `carpricing$CarBrandpeugeot` +  
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_26)
vif(model_26)



#removing CarBranddodge
model_27<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
                `carpricing$CarBrandhonda` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` +  
               `carpricing$CarBrandmitsubishi` + 
               `carpricing$CarBrandpeugeot` +  
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_27)
vif(model_27)


#removing CarBrandhonda
model_28<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + `carpricing$CarBrandchevrolet` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` +  
               `carpricing$CarBrandmitsubishi` + 
               `carpricing$CarBrandpeugeot` +  
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_28)
vif(model_28)



#removing CarBrandchevrolet
model_29<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + 
               `carpricing$CarBrandisuzu` + `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` +  
               `carpricing$CarBrandmitsubishi` + 
               `carpricing$CarBrandpeugeot` +  
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_29)
vif(model_29)

#removing CarBrandisuzu
model_30<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + 
               `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` +  
               `carpricing$CarBrandmitsubishi` + 
               `carpricing$CarBrandpeugeot` +  
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_30)
vif(model_30)

#removing CarBrandmitsubishi
model_31<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + 
               `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandmazda` +  
               `carpricing$CarBrandpeugeot` +  
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_31)
vif(model_31)


#removing CarBrandmazda
model_32<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + 
               `carpricing$CarBrandjaguar` + 
               `carpricing$CarBrandpeugeot` +  
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_32)
vif(model_32)

#removing CarBrandjaguar
model_33<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + 
               `carpricing$CarBrandpeugeot` +  
               `carpricing$CarBrandtoyota` + 
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_33)
vif(model_33)

#removing CarBrandtoyota
model_34<-lm(formula = price ~  enginelocation + 
               curbweight  +  
               `carpricing$cylindernumbermedium` + 
               `carpricing$CarBrandbmw` + `carpricing$CarBrandbuick` + 
               `carpricing$CarBrandpeugeot` +  
               `carpricing$carbodywagon` , 
             data = carpricing_train)
summary(model_34)
vif(model_34)

coefficients(model_34) # model coefficients
confint(model_34, level=0.95) # CIs for model parameters 
fitted(model_34) # predicted values
residuals(model_34) # residuals
anova(model_34) # anova table 
vcov(model_34) # covariance matrix for model parameters 
influence(model_34) # regression diagnostics
layout(matrix(c(1,2,3,4),2,2))
plot(model_34)


Predict_1 <- predict(model_34,carpricing_test[,-18])
carpricing_test$test_price <- Predict_1



# Now, we need to test the r square between actual and predicted sales. 
r <- cor(carpricing_test$price,carpricing_test$test_price)
rsquared <- cor(carpricing_test$price,carpricing_test$test_price)^2
rsquared

#Predictors for the final model(model_34) is 
###1. enginelocation
###2. curbweight
###3. carpricing$cylindernumbermedium('four','five','six')
###4. CarBrandbmw
###5. CarBrandbuick
###6. CarBrandpeugeot
###7. carbodywagon