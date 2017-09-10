#Sourcing data in R
loanData <- read.csv("loan.csv",stringsAsFactors = FALSE)

##########################################################################################################
# ENSURE FOLLOWING LIBRARIES ARE INSTALLED : using install.packages() & restart R studio after insallation
##########################################################################################################

install.packages("dplR")
install.packages("tidyr")
install.packages("ggplot2")

library(dplyr)
library(tidyr)
library(ggplot2)

##################################################
#DATA SLICING : 
# Slicing in to Charged Off and Fully Paid 
# as the current can be either of them at the end
##################################################

loanDataAnalyze<-subset(loanData,loanData$loan_status=="Fully Paid"|loanData$loan_status=="Charged Off")
ncol(loanDataAnalyze)
nrow(loanDataAnalyze)

#########################
##### DATA CLEANING #####
#########################

#Number of columns with all values as NA's
length(colnames(loanDataAnalyze)[colSums(is.na(loanDataAnalyze)) == nrow(loanDataAnalyze)])
#Removing columns with all values as NA
loanDataAnalyze<-loanDataAnalyze[, colSums(is.na(loanDataAnalyze)) != nrow(loanDataAnalyze)]
#Number of columns with all values as 0
length(colnames(loanDataAnalyze)[colSums(loanDataAnalyze == 0,na.rm = TRUE) == nrow(loanDataAnalyze)])
which(colSums(loanDataAnalyze == 0,na.rm = TRUE) == nrow(loanDataAnalyze))

#Removing these FOUR columns [out_prncp  out_prncp_inv acc_now_delinq    delinq_amnt] 
# as there values are same for all rows so they won't be impacting at all
loanDataAnalyze<-loanDataAnalyze[, colSums(loanDataAnalyze == 0,na.rm = TRUE) != nrow(loanDataAnalyze)]

#Removing Columns with more then 95 percent values as empty
which(colSums(loanDataAnalyze == "",na.rm = TRUE) > nrow(loanDataAnalyze)*.95)
loanDataAnalyze<-loanDataAnalyze[, colSums(loanDataAnalyze == "",na.rm = TRUE) < nrow(loanDataAnalyze)*.95]

#Removing desc,url as they are just charater column which should not have an impact
loanDataAnalyze$desc<-NULL
loanDataAnalyze$url<-NULL

#Removing pymnt_plan,initial_list_status,policy_code,application_type 
# as it have only one kind of value for every loan so no need to analyze
length(unique(loanDataAnalyze$pymnt_plan))
length(unique(loanDataAnalyze$initial_list_status))
length(unique(loanDataAnalyze$application_type))
length(unique(loanDataAnalyze$policy_code))
loanDataAnalyze$pymnt_plan<-NULL
loanDataAnalyze$application_type<-NULL
loanDataAnalyze$policy_code<-NULL
loanDataAnalyze$initial_list_status<-NULL

#Removing collections_12_mths_ex_med,tax_liens,chargeoff_within_12_mths 
# as they have only NA's and one unique value
summary(loanDataAnalyze$collections_12_mths_ex_med)
summary(loanDataAnalyze$tax_liens)
summary(loanDataAnalyze$chargeoff_within_12_mths)
loanDataAnalyze$collections_12_mths_ex_med<-NULL
loanDataAnalyze$tax_liens<-NULL
loanDataAnalyze$chargeoff_within_12_mths<-NULL

#Removing mths_since_last_record as >90% of them are NA's
which(colSums(is.na(loanDataAnalyze)) > nrow(loanDataAnalyze)*.90)
loanDataAnalyze$mths_since_last_record<-NULL

#Removing the below attributes as these all are loan 
# attributes which comes into existence
# after the loan is given, so they won't help in knowing 
# the driving factors on the status of loan

drops <- c("emp_title","title","zip_code","last_credit_pull_d","last_pymnt_amnt","last_pymnt_d","collection_recovery_fee")
drops1 <- c("recoveries","total_rec_late_fee","total_rec_int","total_rec_prncp","total_pymnt_inv","total_pymnt")
loanDataAnalyze<-loanDataAnalyze[ , !(names(loanDataAnalyze) %in% drops)]
loanDataAnalyze<-loanDataAnalyze[ , !(names(loanDataAnalyze) %in% drops1)]

#Removing outliers of annual income column, as the 
# mean and median and max shows some outliers exist

summary(loanDataAnalyze$annual_inc)
x<-loanDataAnalyze$annual_inc
qnt <- quantile(x, probs=c(.25, .75))
H <- 3 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
## after executing the following the outlier is removed and the same is written to a new CSV: 
loanDataAnalyze_Cleaned<-subset(loanDataAnalyze,loanDataAnalyze$annual_inc < outlier_value)
View(loanDataAnalyze_Cleaned)



#Converting the Date columns to date type from chr
typeof(loanDataAnalyze_Cleaned$issue_d)
as.Date(loanDataAnalyze_Cleaned$issue_d,format="%b-%y")#returns NA
loanDataAnalyze_Cleaned$issue_d<-paste("01-",loanDataAnalyze_Cleaned$issue_d,sep = "")
loanDataAnalyze_Cleaned$issue_d<-as.Date(loanDataAnalyze_Cleaned$issue_d,format="%d-%b-%y")
loanDataAnalyze_Cleaned$earliest_cr_line<-paste("01-",loanDataAnalyze_Cleaned$earliest_cr_line,sep = "")
loanDataAnalyze_Cleaned$earliest_cr_line<-as.Date(loanDataAnalyze_Cleaned$earliest_cr_line,format="%d-%b-%y")
str(loanDataAnalyze_Cleaned)

#Getting number from the term and interest column for further analysis
loanDataAnalyze_Cleaned$term<-as.numeric(gsub("([0-9]+).*$", "\\1", loanDataAnalyze_Cleaned$term))
loanDataAnalyze_Cleaned$int_rate<-as.numeric(sub("%","", loanDataAnalyze_Cleaned$int_rate))
loanDataAnalyze_Cleaned$annual_inc<-round(loanDataAnalyze_Cleaned$annual_inc,0)
#Removing year and years from emp_length column
loanDataAnalyze_Cleaned$emp_length<-sub("years","", loanDataAnalyze_Cleaned$emp_length)
loanDataAnalyze_Cleaned$emp_length<-sub("year","", loanDataAnalyze_Cleaned$emp_length)
#Converting dti column from percentage to number
loanDataAnalyze_Cleaned$dti<-loanDataAnalyze_Cleaned$dti/100

# write the cleaned up CSV 
write.csv(loanDataAnalyze_Cleaned,file = "loanCleanedNewFinal.csv")


##### END OF DATA CLEANUP #####


####################
##### ANALYSIS #####
####################

#Univariate/Segmented Univariate

#1. #funded_amnt,funded_amnt_inv,funded_amnt_lc(derived)
length(which(loanDataAnalyze_Cleaned$funded_amnt>loanDataAnalyze_Cleaned$loan_amnt))
LoanStatus_group<- group_by(loanDataAnalyze_Cleaned,loan_status)
funded_amnt_summarize<-summarise(LoanStatus_group,avg.funded_amnt=mean(funded_amnt))

#Difference of more then 1k between the average funded amount of Charged off loans and fully paid loans
ggplot(funded_amnt_summarize,aes(x=loan_status,y=avg.funded_amnt))+geom_point()
summary(funded_amnt_summarize)

#Deriving one column
loanDataAnalyze_Cleaned$funded_amnt_lc<-loanDataAnalyze_Cleaned$funded_amnt - loanDataAnalyze_Cleaned$funded_amnt_inv
LoanStatus_group<- group_by(loanDataAnalyze_Cleaned,loan_status)
funded_amnt_lc_summarize<-summarise(LoanStatus_group,avg.funded_amnt_lc=mean(funded_amnt_lc))
summary(funded_amnt_lc_summarize)
#Difference of around 300 between the amount invested by LC for charged off and fully paid loans

#2 Term int rate and funded amnt relation
LoanTermRate_group<- group_by(loanDataAnalyze_Cleaned,term,loan_status)
funded_amnt_term_Status_summarize<-summarise(LoanTermRate_group,avg.funded_amnt=mean(funded_amnt),avg.int_rate=mean(int_rate))
intrate_summarize<-summarise(LoanStatus_group,avg.rate=mean(int_rate))
loanDataAnalyze.Term<-ggplot(loanDataAnalyze, aes(x = factor(term),y = (..count..)/sum(..count..),fill=factor(loan_status)))
loanDataAnalyze.Term+geom_bar()+xlab("Term") +ylab("Percentage of Loans")+stat_count(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1) +scale_y_continuous(labels = scales::percent)
#Percentage of 36 months term loan going to default is 2 more then 60 months as giving amount in less time is always a trouble 
#the table shows that the interest rate and loan amount fundned both are less for 36 months term loan but still they default more

#3 Grade, int rate and funded amnt relation
loanDataAnalyze.Grade<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(grade),y = (..count..)/sum(..count..),fill=factor(loan_status)))
loanDataAnalyze.Grade+geom_bar()+xlab("Grade") +ylab("Percentage of Loans")+stat_count(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1) +scale_y_continuous(labels = scales::percent)
#Grade B and C loans default the most among other grade loans
LoanGrade_group<- group_by(loanDataAnalyze_Cleaned,grade)
LoanGradeTerm_group<- group_by(loanDataAnalyze_Cleaned,grade,term)
funded_amnt_grade_summarize<-summarise(LoanGrade_group,avg.funded_amnt=mean(funded_amnt),avg.int_rate=mean(int_rate))
funded_amnt_grade_term_summarize<-summarise(LoanGradeTerm_group,avg.funded_amnt=mean(funded_amnt),avg.int_rate=mean(int_rate))
#As the grade increases the avg funded amount and avg interest rate also increases, Grade B loans provide
#higher loan amount at comparitively lesser interest rate so more people tend to take it and more people tend to default
#There is 2 percent difference between charged off and fully paid loans, this shows higher the interest rate higher the default

#Installment is proportional to interest rate
installment_summarize<-summarise(LoanStatus_group,avg.installment=mean(installment))

#4. emp_length and anuual_income
#Handling annual income outliers or there is no need to remove outliers if I am taking medain while summarizing
LoanEmp_length_group<- group_by(loanDataAnalyze_Cleaned,emp_length)
loanDataAnalyze.emp_length<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(emp_length),y = (..count..)/sum(..count..),fill=factor(loan_status)))
loanDataAnalyze.emp_length+geom_bar()+xlab("Emp Length") +ylab("Percentage of Loans")+stat_count(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1) +scale_y_continuous(labels = scales::percent)
#Graph shows that the people with experince greater then 10 tend to defalut more
#This might be because they have more responsibities then the less experinced guys
LoanEmp_length_summarize<-summarise(LoanEmp_length_group,avg.income=mean(annual_inc),dti=mean(dti))
#Summmariztion shows that they have more annual income then other groups but still they default more
annual_income_summarize<-summarise(LoanStatus_group,avg.amount=mean(annual_inc))
#Summarization shows that there is a difference of 7k in the annual income of the charged off and fully paid loans
ggplot(loanDataAnalyze_Cleaned,aes(x=loan_status,y=annual_inc))+xlab("Status") +ylab("Income")+geom_boxplot()+facet_grid(~loan_status)


#5. home_ownership
loanDataAnalyze.home_ownership<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(home_ownership),y = (..count..)/sum(..count..),fill=factor(loan_status)))
loanDataAnalyze.home_ownership+geom_bar()+xlab("Ownership") +ylab("Loan Percentage")+stat_count(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1) +scale_y_continuous(labels = scales::percent)
Loanhome_ownership_group<- group_by(loanDataAnalyze_Cleaned,home_ownership)
Loanhome_ownership_group_summarize<-summarise(Loanhome_ownership_group,avg.income=mean(annual_inc),dti=mean(dti))
#graph shows the most default are on RENT ownership as the annual income average comes out to be less for them.

#5.#purpose
loanDataAnalyze.Purpose<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(purpose),y = (..count..)/sum(..count..),fill=factor(loan_status)))
loanDataAnalyze.Purpose+geom_bar(position = "dodge")+xlab("Purpose") +ylab("Loan Percentage")+stat_count(geom = "text",position=position_dodge(width=0.9),aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = -0.25) +scale_y_continuous(labels = scales::percent)+scale_x_discrete(labels = abbreviate)
Loanpurpose_group<- group_by(loanDataAnalyze_Cleaned,purpose)
Loanpurpose_group_summarize<-summarise(Loanpurpose_group,avg.income=mean(annual_inc),rate=mean(int_rate),dti=mean(dti))
#Graph shows that most of the loan is taken for debt_consolidation and default percentage is also more
#Most of the people with home ownership as rented take loan for debt consolidation and their average annual income is less

#6. Verification Status
loanDataAnalyze.Verification<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(verification_status),y = (..count..)/sum(..count..),fill=factor(loan_status),position="dodge"))
loanDataAnalyze.Verification+geom_bar()+xlab("Verification Status") +ylab("Loan Percentage")+stat_count(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1) +scale_y_continuous(labels = scales::percent)
#Loans with income source not verified tend to default more then the source verified

#7. Issue Date
#Derived metrics Issue Date month
loanDataAnalyze_Cleaned$issue_d_mon<-as.numeric(format(loanDataAnalyze_Cleaned$issue_d,'%m'))
loanDataAnalyze.issue_d_mon<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(issue_d_mon),y = (..count..)/sum(..count..),fill=factor(loan_status)))
loanDataAnalyze.issue_d_mon+geom_bar()+xlab("Verification Status") +ylab("Loan Percentage")+stat_count(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1) +scale_y_continuous(labels = scales::percent)

LoanDate_group<- group_by(loanDataAnalyze_Cleaned,issue_d_mon)
LoanDate_group_summarize<-summarise(LoanDate_group,avg.income=mean(annual_inc),rate=mean(int_rate),dti=mean(dti))
#The default rate increases for the loans issued in moth of november december


#8 Total Acc and Open Acc Analysis
open_acc_summarize<-summarise(LoanStatus_group,avg.rate=mean(open_acc))
total_acc_summarize<-summarise(LoanStatus_group,avg.rate=mean(total_acc))
#Derived metrics
loanDataAnalyze_Cleaned$closed_acc<-loanDataAnalyze_Cleaned$total_acc - loanDataAnalyze_Cleaned$open_acc
LoanStatus_group<- group_by(loanDataAnalyze_Cleaned,loan_status)
closed_acc_summarize<-summarise(LoanStatus_group,avg.rate=mean(closed_acc))

#9. Revol bal, Revol Util 
revol_bal_summarize<-summarise(LoanStatus_group,avg.amount=mean(revol_bal))
loanDataAnalyze_Cleaned$revol_util<-as.numeric(sub("%","", loanDataAnalyze_Cleaned$revol_util))
loanDataAnalyze_Cleaned$revol_util_bal<-loanDataAnalyze_Cleaned$revol_bal*loanDataAnalyze_Cleaned$revol_util/100
LoanStatus_group<- group_by(loanDataAnalyze_Cleaned,loan_status)
revol_util_bal_summarize<-summarise(LoanStatus_group,avg.amount=mean(revol_util_bal,na.rm=TRUE))

#Dti
dti_summarize<-summarise(LoanStatus_group,avg.amount=mean(dti))

#10 pub_rec_bankruptcies#No Impact as the number of 1s is very less then number of 0's
cor(loanDataAnalyze$annual_inc,loanDataAnalyze$pub_rec_bankruptcies)
library(ggplot2)
loanDataAnalyze.pub_rec_bankruptcies<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(pub_rec_bankruptcies)))
loanDataAnalyze.pub_rec_bankruptcies+geom_bar()
loanDataAnalyze.pub_rec_bankruptcies<-loanDataAnalyze.pub_rec_bankruptcies+aes(fill=factor(loan_status))
loanDataAnalyze.pub_rec_bankruptcies+geom_bar()
#Rmoving it as the 0 values is very much to analyze
loanDataAnalyze_Cleaned$pub_rec_bankruptcies<-NULL
View(loanDataAnalyze_Cleaned)


#11. addr_state
loanDataAnalyze.addr_state<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(addr_state),y = (..count..)/sum(..count..),fill=factor(loan_status),position="dodge"))
loanDataAnalyze.addr_state+geom_bar()+stat_count(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = .5) +scale_y_continuous(labels = scales::percent)+scale_x_discrete(labels = abbreviate)
Loanstate_group<- group_by(loanDataAnalyze_Cleaned,addr_state)
Loanstate_group_summarize<-summarise(Loanstate_group,avg.income=mean(annual_inc),rate=mean(int_rate),dti=mean(dti))


#12.  pub_rec
loanDataAnalyze.pub_rec<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(pub_rec),y = (..count..)/sum(..count..),fill=factor(loan_status)))
loanDataAnalyze.pub_rec+geom_bar()+stat_count(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1) +scale_y_continuous(labels = scales::percent)+scale_x_discrete(labels = abbreviate)
#Rmoving it as the 0 values is very much to analyze
loanDataAnalyze_Cleaned$pub_rec<-NULL

#13.  delinq_2yrs
loanDataAnalyze.delinq_2yrs<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(delinq_2yrs),y = (..count..)/sum(..count..),fill=factor(loan_status)))
loanDataAnalyze.delinq_2yrs+geom_bar()+stat_count(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1) +scale_y_continuous(labels = scales::percent)+scale_x_discrete(labels = abbreviate)
#Rmoving it as the 0 values is very much to analyze
loanDataAnalyze_Cleaned$delinq_2yrs<-NULL


#13.  inq_last_6mths
loanDataAnalyze.inq_last_6mths<-ggplot(loanDataAnalyze_Cleaned, aes(x = factor(inq_last_6mths),y = (..count..)/sum(..count..),fill=factor(loan_status)))
loanDataAnalyze.inq_last_6mths+geom_bar()+xlab("No. Of Inquiries") +ylab("Loan Percentage")+stat_count(geom = "text",aes(label = paste(round((..count..)/sum(..count..)*100), "%")),vjust = 1) +scale_y_continuous(labels = scales::percent)+scale_x_discrete(labels = abbreviate)
#Rmoving it as the 0 values is very much to analyze
#As the inquiry goes up the chances of default goes less

