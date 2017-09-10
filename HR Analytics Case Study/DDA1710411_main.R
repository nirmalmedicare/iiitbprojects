library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(lubridate)
library(scales)
library(gridExtra)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)

#setwd("C:/Users/Priy/Desktop/IIITB/Modelling/Logistic Regression")
##############Loading the data 
employee_survey_data<- read.csv(file="employee_survey_data.csv",stringsAsFactors = FALSE)
general_data<- read.csv(file="general_data.csv",stringsAsFactors = FALSE)
manager_survey_data<- read.csv(file="manager_survey_data.csv",stringsAsFactors = FALSE)

##############Checking if there is any difference in the employee id's of the file to merge
setdiff(employee_survey_data$EmployeeID,general_data$EmployeeID)
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID)

##############As the differences are 0 merging the files into 1
employee_data<- merge(employee_survey_data,general_data, by="EmployeeID", all = F)
employee_data<- merge(employee_data,manager_survey_data, by="EmployeeID", all = F)

##############Loading the time files
intime<-read.csv(file="in_time.csv",stringsAsFactors = FALSE)
outtime<-read.csv(file="out_time.csv",stringsAsFactors = FALSE)

##############Changing the names of the column in the 2 files
colnames(intime)[1]<-c("EmployeeID")
colnames(outtime)[1]<-c("EmployeeID")

##############Changing the Wide format to Long format
intime <- gather(intime,date,timein,-EmployeeID)
outtime<-gather(outtime,date,timeout,-EmployeeID)

##############Merging the in and out files
timediff_file<-merge(intime,outtime,by=c("EmployeeID","date"))

##############Checking whether there are some cases in which employee missed to punch in or out
sum(is.na(timediff_file$timein) & !is.na(timediff_file$timeout))
sum(!is.na(timediff_file$timein) & is.na(timediff_file$timeout))

##############Converting the time variables to date format and finding the time difference variable
timediff_file$timein <- as.POSIXlt(timediff_file$timein, format = "%Y-%m-%d %H:%M:%S")
timediff_file$timeout <- as.POSIXlt(timediff_file$timeout, format = "%Y-%m-%d %H:%M:%S")
timediff_file$timeDifference<-difftime(timediff_file$timeout,timediff_file$timein)
timediff_file$timeDifference<-as.numeric(timediff_file$timeDifference)
timediff_file<-timediff_file[,-c(3,4)]
timediff_file<-spread(timediff_file,date,timeDifference)
timediff_file[,2:262]<-sapply(timediff_file[,2:262],function(x) as.integer(as.character(x)))


##############Number of columns with all values as NA's
length(colnames(timediff_file)[colSums(is.na(timediff_file)) == nrow(timediff_file)])
##############Removing columns with all values as NA
timediff_file<-timediff_file[, colSums(is.na(timediff_file)) != nrow(timediff_file)]

##############Merging both the files after checking the difference
setdiff(employee_data$EmployeeID,timediff_file$EmployeeID)

employee_data_final<- merge(employee_data,timediff_file, by="EmployeeID", all = F)

##############Checking the NA values in each column
employee_data_final %>%
  summarise_all(funs(sum(is.na(.))))

length(unique(employee_data_final$Attrition))

##############Removing rows with NA values of Total working years and NumCompaniesWorked as they are only 9 and 19
empTotWorkYrRemove<-c(which(is.na(employee_data_final$TotalWorkingYears)))
employee_data_final<-employee_data_final[-empTotWorkYrRemove,]

empNumCompaniesWorkedRemove<-c(which(is.na(employee_data_final$NumCompaniesWorked)))
employee_data_final<-employee_data_final[-empNumCompaniesWorkedRemove,]

##############Removing EmployeeCount column as it has only one value for all rows
unique(employee_data_final$EmployeeCount)
employee_data_final$EmployeeCount<- NULL

##############Removing Over18 column as it has only one value for all rows
unique(employee_data_final$Over18)
employee_data_final$Over18<- NULL

##############Removing StandardHours column as it has only one value for all rows
unique(employee_data_final$StandardHours)
employee_data_final$StandardHours<- NULL

##############Function to get the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Imputation of values which are still NA's, Imputing them with the mode of that particular column 
employee_data_final[which(is.na(employee_data_final$EnvironmentSatisfaction)),]$EnvironmentSatisfaction<-getmode(employee_data_final$EnvironmentSatisfaction)
employee_data_final[which(is.na(employee_data_final$JobSatisfaction)),]$JobSatisfaction<-getmode(employee_data_final$JobSatisfaction)
employee_data_final[which(is.na(employee_data_final$WorkLifeBalance)),]$WorkLifeBalance<-getmode(employee_data_final$WorkLifeBalance)

#No Na's Left in the data Except the date column


str(employee_data_final)
#Checking Outliers in the continuos data 



# Histogram and Boxplots for numeric variables for checking outliers
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

#No outliers in Age
quantile(employee_data_final$Age,seq(0,1,0.01))
plot_grid(ggplot(employee_data_final, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(employee_data_final, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)



#No outliers in DistanceFromHome
quantile(employee_data_final$DistanceFromHome,seq(0,1,0.01))
summary(employee_data_final$DistanceFromHome)


#Treatment of Outliers in Monthly Income
quantile(employee_data_final$MonthlyIncome,seq(0,1,0.01))
plot_grid(ggplot(employee_data_final, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(employee_data_final, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

x<-employee_data_final$MonthlyIncome
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
employee_data_final$MonthlyIncome[which(employee_data_final$MonthlyIncome>outlier_value)]<-outlier_value

#No outliers in PercentSalaryHike
quantile(employee_data_final$PercentSalaryHike,seq(0,1,0.01))
plot_grid(ggplot(employee_data_final, aes(PercentSalaryHike))+ geom_histogram(binwidth = 5),
          ggplot(employee_data_final, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

#outliers in TotalWorkingYears
quantile(employee_data_final$TotalWorkingYears,seq(0,1,0.01))
plot_grid(ggplot(employee_data_final, aes(TotalWorkingYears))+ geom_histogram(binwidth = 5),
          ggplot(employee_data_final, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

x<-employee_data_final$TotalWorkingYears
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
employee_data_final$TotalWorkingYears[which(employee_data_final$TotalWorkingYears>outlier_value)]<-outlier_value



#Removing outliers in YearsAtCompany
quantile(employee_data_final$YearsAtCompany,seq(0,1,0.01))
plot_grid(ggplot(employee_data_final, aes(YearsAtCompany))+ geom_histogram(binwidth = 5),
          ggplot(employee_data_final, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

x<-employee_data_final$YearsAtCompany
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
employee_data_final$YearsAtCompany[which(employee_data_final$YearsAtCompany>outlier_value)]<-outlier_value


#Discrepancy in NumCompaniesWorked column
#Assuming that if Current Company is not counted in the NumCompaniesWorked
#So if the Total Working years are equal to Years at company replacing it with 0
#If the difference is 1 replacing it with 1 as there are only 1 and 0 values for Num companies worked if the difference is 1
length(which(employee_data_final$TotalWorkingYears<employee_data_final$YearsAtCompany))
c<-employee_data_final[which(employee_data_final$TotalWorkingYears-employee_data_final$YearsAtCompany==1),]$NumCompaniesWorked
unique(c)
employee_data_final[which(employee_data_final$TotalWorkingYears-employee_data_final$YearsAtCompany==1),]$NumCompaniesWorked<-1

employee_data_final[which(employee_data_final$TotalWorkingYears==employee_data_final$YearsAtCompany),]$NumCompaniesWorked<-0

#No outliers in NumCompaniesWorked
quantile(employee_data_final$NumCompaniesWorked,seq(0,1,0.01))
plot_grid(ggplot(employee_data_final, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 5),
          ggplot(employee_data_final, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

str(employee_data_final)

#Removing outliers in YearsSinceLastPromotion
quantile(employee_data_final$YearsSinceLastPromotion,seq(0,1,0.01))
plot_grid(ggplot(employee_data_final, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 5),
          ggplot(employee_data_final, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

x<-employee_data_final$YearsSinceLastPromotion
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
employee_data_final$YearsSinceLastPromotion[which(employee_data_final$YearsSinceLastPromotion>outlier_value)]<-outlier_value


#Removing outliers in YearsWithCurrManager
quantile(employee_data_final$YearsWithCurrManager,seq(0,1,0.01))
plot_grid(ggplot(employee_data_final, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 5),
          ggplot(employee_data_final, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)
x<-employee_data_final$YearsWithCurrManager
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
employee_data_final$YearsWithCurrManager[which(employee_data_final$YearsWithCurrManager>outlier_value)]<-outlier_value


#Converting categorical variables using factor command and binning wherever necessary.
employee_data_final$EnvironmentSatisfaction<-as.factor(employee_data_final$EnvironmentSatisfaction)
employee_data_final$Attrition<-as.factor(employee_data_final$Attrition)
employee_data_final$JobSatisfaction<-as.factor(employee_data_final$JobSatisfaction)
employee_data_final$WorkLifeBalance<-as.factor(employee_data_final$WorkLifeBalance)
employee_data_final$BusinessTravel<-as.factor(employee_data_final$BusinessTravel)
employee_data_final$Department<-as.factor(employee_data_final$Department)
employee_data_final$Education<-as.factor(employee_data_final$Education)
employee_data_final$EducationField<-as.factor(employee_data_final$EducationField)
employee_data_final$Gender<-as.factor(employee_data_final$Gender)
employee_data_final$JobLevel<-as.factor(employee_data_final$JobLevel)
employee_data_final$JobRole<-as.factor(employee_data_final$JobRole)
employee_data_final$MaritalStatus<-as.factor(employee_data_final$MaritalStatus)
employee_data_final$JobInvolvement<-as.factor(employee_data_final$JobInvolvement)
employee_data_final$PerformanceRating<-as.factor(employee_data_final$PerformanceRating)
employee_data_final$TrainingTimesLastYear<-as.factor(employee_data_final$TrainingTimesLastYear)
employee_data_final$StockOptionLevel<-as.factor(employee_data_final$StockOptionLevel)

#Deriving new columns numberOfLeaves and Average Time worked from the date columns
#As each row contains NA's only in the date column we can make the number of leaves as count of NA's in a row.
employee_data_final$leaves <- apply(employee_data_final, 1, function(x) sum(is.na(x)))
employee_data_final$averageTimeSpent<-rowMeans(employee_data_final[,27:275], na.rm = TRUE, dims = 1)
employee_data_final$averageTimeSpent<-as.integer(employee_data_final$averageTimeSpent)
typeof(employee_data_final$averageTimeSpent)
employee_data_final[,27:275]<-NULL
str(employee_data_final)

#No outliers in leaves
quantile(employee_data_final$leaves,seq(0,1,0.01))
plot_grid(ggplot(employee_data_final, aes(leaves))+ geom_histogram(binwidth = 5),
          ggplot(employee_data_final, aes(x="",y=leaves))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

#No outliers in averageTimeSpent
quantile(employee_data_final$averageTimeSpent,seq(0,1,0.01))
plot_grid(ggplot(employee_data_final, aes(averageTimeSpent))+ geom_histogram(binwidth = 5),
          ggplot(employee_data_final, aes(x="",y=averageTimeSpent))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


#EDA for continuous variables
str(employee_data_final)
# Boxplots of numeric variables relative to attrition status
# The plot shows that the Age of the people who resgins from a company goes in the lower Age bracket
# The 50-75 percentile of the person who resigns have to travel more distance from home
plot_grid(ggplot(employee_data_final, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data_final, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)


# Plot shows as expected that the monthly income for the persons who resgins
#are in less bracket than who status with the company and the num of company bracket
#is more for the person who resigned
plot_grid(ggplot(employee_data_final, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data_final, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Percent salay hike does not matter that much
plot_grid(ggplot(employee_data_final, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"))

# Plot shows that the less experinced people resign more 
# and the person who have spent less years with the company resigned more
plot_grid(ggplot(employee_data_final, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data_final, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

# Plot shows that the promotion bracket for the person who leaves the company
# is less the the person who stays and the years with current manager is also less
# as expected for the person who leaves the company
plot_grid(ggplot(employee_data_final, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data_final, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

#The below plot shows interestingly that the average time spent by
#the persons who leave the company is more than the person who stays
plot_grid(ggplot(employee_data_final, aes(x=Attrition,y=leaves, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employee_data_final, aes(x=Attrition,y=averageTimeSpent, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)

library(GGally)
ggpairs(employee_data_final[, c("Age","YearsWithCurrManager","averageTimeSpent","leaves", "DistanceFromHome", "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", "TotalWorkingYears", "YearsAtCompany", "YearsSinceLastPromotion")])
#Correlation chart shows that there is high positive correlation betweem
#YearsAtCompany column and YearsWithCurrManager column 
# YearsWithCurrManager and YearsSinceLastPromotion column
#TotalWorkingYears and YearsAtCompany column


#EDA for categorical variables

# Barcharts for categorical features with stacked attrition information
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



plot_grid(ggplot(employee_data_final, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1, 
          ggplot(employee_data_final, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data_final, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data_final, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data_final, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data_final, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

plot_grid(ggplot(employee_data_final, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1, 
          ggplot(employee_data_final, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data_final, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data_final, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data_final, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employee_data_final, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

plot_grid(ggplot(employee_data_final, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1, 
          ggplot(employee_data_final, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")


#reveals moderate contrast for atrrition wrt EnvironmentSatisfaction,BusinessTravel and education
#Strong contrast with JobRole and JobLevel and EducationField and JobInvolvement and PerformanceRating 
#

# Normalising continuous features 
?scale
employee_data_final$averageTimeSpent<- scale(employee_data_final$averageTimeSpent) 
employee_data_final$leaves<- scale(employee_data_final$leaves) 
employee_data_final$YearsWithCurrManager<- scale(employee_data_final$YearsWithCurrManager)
employee_data_final$YearsSinceLastPromotion<- scale(employee_data_final$YearsSinceLastPromotion) 
employee_data_final$YearsAtCompany<- scale(employee_data_final$YearsAtCompany) 
employee_data_final$TotalWorkingYears<- scale(employee_data_final$TotalWorkingYears) 
employee_data_final$PercentSalaryHike<- scale(employee_data_final$PercentSalaryHike) 
employee_data_final$NumCompaniesWorked<- scale(employee_data_final$NumCompaniesWorked) 
employee_data_final$MonthlyIncome<- scale(employee_data_final$MonthlyIncome) 
employee_data_final$DistanceFromHome<- scale(employee_data_final$DistanceFromHome) 
employee_data_final$Age<- scale(employee_data_final$Age) 

# converting target variable employee_data_final from No/Yes character to factorwith levels 0/1 
employee_data_final$Attrition<- ifelse(employee_data_final$Attrition=="Yes",1,0)

# Checking attrition rate of prospect employee

Attrition <- sum(employee_data_final$Attrition)/nrow(employee_data_final)
Attrition # 16.08% churn rate. 

# creating a dataframe of categorical features
employee_data_final_chr<- employee_data_final[,-c(1,5,6,9,16,17,18,20,22,23,24,27,28)]

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee_data_final_chr, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_data_final_chr))[,-1]))

# Final dataset
employee_data_model<- cbind(employee_data_final[,c(5,6,9,16,17,18,20,22,23,24,27,28)],dummies) 
View(employee_data_model)
str(employee_data_model)


#Modelling
########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(employee_data_model$Attrition, SplitRatio = 0.7)

train = employee_data_model[indices,]
Attrition_train <- sum(train$Attrition)/nrow(train)
Attrition_train # 16.08% churn rate. 

test = employee_data_model[!(indices),]
Attrition_test <- sum(test$Attrition)/nrow(test)
Attrition_test # 16.08% churn rate.
########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2111.1.....nullDev 2704.5...resDev 1987.1

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
vif(model_2)

#Excluding EducationField.xMedical as it has high VIF and low significance
model_3<- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.x5 + EducationField.xLife.Sciences + 
                EducationField.xMarketing + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Representative + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + StockOptionLevel.x1 + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                JobInvolvement.x2 + JobInvolvement.x3, family = "binomial", 
              data = train)

summary(model_3) 

vif(model_3) 


# Excluding MaritalStatus.xMarried as high VIF and low significance
model_4<- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                Department.xSales + Education.x5 + EducationField.xLife.Sciences + 
                EducationField.xMarketing + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                JobInvolvement.x2 + JobInvolvement.x3, family = "binomial", 
              data = train)

summary(model_4)

vif(model_4) 

#Excluding BusinessTravel.xTravel_Rarely as high VIF and low significance
model_5<- glm(formula = Attrition ~ Age + MonthlyIncome + NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
              Department.xResearch...Development + 
                Department.xSales + Education.x5 + EducationField.xLife.Sciences + 
                EducationField.xMarketing + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Representative + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                JobInvolvement.x2 + JobInvolvement.x3, family = "binomial", 
              data = train) 

summary(model_5) 
vif(model_5)
#Cannot remove anyhting now because of high VIF as they have high significance also
#Removing variables with lower significance
#Excluding MonthlyIncome  due to lower significance
model_6<-glm(formula = Attrition ~ Age +NumCompaniesWorked + 
               TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
               averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
               Department.xResearch...Development + 
               Department.xSales + Education.x5 + EducationField.xLife.Sciences + 
               EducationField.xMarketing + EducationField.xOther + 
               EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
               JobRole.xResearch.Director + JobRole.xSales.Representative + 
               MaritalStatus.xSingle + StockOptionLevel.x1 + 
               TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
               JobInvolvement.x2 + JobInvolvement.x3, family = "binomial", 
             data = train) 
summary(model_6)

#Excluding Education.x5   due to lower significance
model_7<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + 
                Department.xSales  + EducationField.xLife.Sciences + 
                EducationField.xMarketing + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Representative + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                JobInvolvement.x2 + JobInvolvement.x3, family = "binomial", 
              data = train)

summary(model_7) 

#Excluding EducationField.xLife.Sciences  due to lower significance with respect to other 10 variables
model_8<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + 
                Department.xSales  + 
                EducationField.xMarketing + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Representative + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                JobInvolvement.x2 + JobInvolvement.x3, family = "binomial", 
              data = train)

summary(model_8) 


#Excluding EducationField.xMarketing due to lower significance with respect to other 10 variables
model_9<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + 
                Department.xSales  + 
                EducationField.xOther + 
                EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Representative + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                JobInvolvement.x2 + JobInvolvement.x3, family = "binomial", 
              data = train)

summary(model_9) 

#Excluding EducationField.xOther    due to lower significance with respect to other 10 variables
model_10<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales  + 
                 EducationField.xTechnical.Degree + JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director + JobRole.xSales.Representative + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                 JobInvolvement.x2 + JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_10) 


#Excluding EducationField.xTechnical.Degree  due to lower significance with respect to other 10 variables
model_11<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + 
                Department.xSales  + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Representative + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                JobInvolvement.x2 + JobInvolvement.x3, family = "binomial", 
              data = train)

summary(model_11) 


#Excluding JobInvolvement.x2  due to lower significance with respect to other 10 variables
model_12<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + 
                Department.xSales  + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Representative + 
                MaritalStatus.xSingle + StockOptionLevel.x1 + 
                TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                 JobInvolvement.x3, family = "binomial", 
              data = train)

summary(model_12) 
#Excluding JobRole.xSales.Representative  due to lower significance with respect to other 10 variables
model_13<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales  + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                 JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_13)

#Excluding TrainingTimesLastYear.x4  due to lower significance with respect to other 10 variables
model_14<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales  + 
                 JobLevel.x5 + JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 + TrainingTimesLastYear.x5 + TrainingTimesLastYear.x6 + 
                 JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_14)


#Excluding TrainingTimesLastYear.x5  due to lower significance with respect to other 10 variables
model_15<-  glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales  + 
                  JobLevel.x5 + JobRole.xManufacturing.Director + 
                  JobRole.xResearch.Director +
                  MaritalStatus.xSingle + StockOptionLevel.x1 + 
                  +  TrainingTimesLastYear.x6 + 
                  JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_15)

#Excluding JobLevel.x5   due to lower significance with respect to other 10 variables
model_16<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales  + 
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 TrainingTimesLastYear.x6 + 
                 JobInvolvement.x3, family = "binomial", 
               data = train)

summary(model_16)


#Excluding JobInvolvement.x3  due to lower significance with respect to other 10 variables
model_17<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales  + 
                 JobRole.xManufacturing.Director + 
                 JobRole.xResearch.Director +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 TrainingTimesLastYear.x6 , family = "binomial", 
               data = train)

summary(model_17)


#Excluding JobRole.xResearch.Director  due to lower significance with respect to other 10 variables
model_18<-  glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales  + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle + StockOptionLevel.x1 + 
                  TrainingTimesLastYear.x6 , family = "binomial", 
                data = train)

summary(model_18)

#Excluding StockOptionLevel.x1  due to lower significance with respect to other 10 variables
model_19<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales  + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle +  
                 TrainingTimesLastYear.x6 , family = "binomial", 
               data = train)

summary(model_19)

#Excluding WorkLifeBalance.x4  due to lower significance with respect to other 10 variables
model_20<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales  + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle +  
                 TrainingTimesLastYear.x6 , family = "binomial", 
               data = train)

summary(model_20)

#Excluding WorkLifeBalance.x2     due to lower significance with respect to other 10 variables
model_21<-  glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales  + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle +  
                  TrainingTimesLastYear.x6 , family = "binomial", 
                data = train)

summary(model_21)


#Excluding TrainingTimesLastYear.x6        due to lower significance with respect to other 10 variables
model_22<-  glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                  averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently + 
                  Department.xResearch...Development + 
                  Department.xSales  + 
                  JobRole.xManufacturing.Director + 
                  MaritalStatus.xSingle  , family = "binomial", 
                data = train)

summary(model_22)


#Excluding JobSatisfaction.x2        due to lower significance with respect to other 10 variables
model_23<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales  + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle  , family = "binomial", 
               data = train)

summary(model_23)


#Excluding JobSatisfaction.x3       due to lower significance with respect to other 10 variables
model_24<- glm(formula = Attrition ~ Age +NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 +  
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales  + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle  , family = "binomial", 
               data = train)

summary(model_24)

#Excluding Age due to lower significance with respect to other 10 variables
model_25<- glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + 
                 averageTimeSpent + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 +  
                 JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                 BusinessTravel.xTravel_Frequently + 
                 Department.xResearch...Development + 
                 Department.xSales  + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle  , family = "binomial", 
               data = train)

summary(model_25)
#No more variables can be removed as all are of higher significance
#15 variables forund to be significant in thee final model
#######################################################################
final_model<- model_25
### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_Attrition<- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
test_conf

table(test_actual_Attrition,test_pred_Attrition)

#Sensitivity coming as very high with cutoff of 50 percent
#Accuracy : 0.8456   
#Sensitivity : 0.19811         
#Specificity : 0.97008       

#######################################################################
# Let's use the probability cutoff of 40%.
test_pred_Attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
test_conf

# Accuracy : 0.8494 
# Sensitivity : 0.33019         
# Specificity : 0.94923    

# Let's use the probability cutoff of 25%.
test_pred_Attrition <- factor(ifelse(test_pred >= 0.25, "Yes", "No"))

test_conf <- confusionMatrix(test_pred_Attrition, test_actual_Attrition, positive = "Yes")
test_conf

# Accuracy : 0.7962  
# Sensitivity :0.53302         
#Specificity : 0.84678  
#######################################################################

#########################################################################################

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]



# Let's choose a cutoff value of 0.1776 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1776, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_Attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec
# Accuracy 
# 0.7361217 
# Sensitivity 
# 0.7311321 
# Specificity 
# 0.737080
View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_Attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_Attrition, test_pred, groups = 10)




