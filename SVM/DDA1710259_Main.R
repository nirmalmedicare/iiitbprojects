library(gridExtra)
library(readr)
library(dplyr)
library(e1071)
library(caret)
library(kernlab)
library(ggplot2)
library(MASS)
library(caTools)
#####################################################################
#Data Understanding
#Excluding one label column in the data which shows what kind of digit it is
#the data should be treated as a matrix of 28*28 dimensions each row is the 
#attribute of the digit
#####################################################################
setwd("C:/Users/Priy/Desktop/IIITB/Modelling/SVM")

#####################################################################
#Reading of the data into R
#####################################################################
mnsit_train<- read.csv("mnist_train.csv",header = FALSE)
str(mnsit_train)

mnist_test<-read.csv("mnist_test.csv",header = FALSE)

colnames(mnsit_train)[1]<-c("label")
colnames(mnist_test)[1]<-c("label")


#####################################################################
#Some EDA about the data
#####################################################################

# Checking missing value, No missing value
sapply(mnsit_train, function(x) sum(is.na(x)))

# Checking outlier value, No outlier values
sapply(mnsit_train, function(x) quantile(x,seq(0,1,0.01)))

#Checking the different digits frequency in the data
ggplot(mnsit_train,aes(x=as.factor(label),fill=label))+
  geom_bar(stat="count")+
  labs(title="Digits",x="Digits")

#Plotting one of the digit of one of the row
?matrix

digit <- matrix(as.numeric(mnsit_train[8,-1]), nrow = 28) #look at one digit

?image
image(digit, col = grey.colors(255))

#####################################################################
#Will perform PCA for dimensionality reduction
#Before performing PCA , we need to remove the columns which has zero variance or
#entire data as same,we need to remove these columns using zero variance function
#####################################################################

nearzv<- nearZeroVar(mnsit_train[,-1],saveMetrics = TRUE)
head(nearzv)
range(nearzv$percentUnique)
sum(nearzv$zeroVar)
sum(nearzv$nzv)
removeCols <- rownames(nearzv[nearzv$nzv==TRUE,])
variation <- setdiff(names(mnsit_train),removeCols)
mnsit_train_nzv <- mnsit_train[,variation]

label <- as.factor(mnsit_train_nzv[[1]])
mnsit_train_nzv$label <- NULL

#####################################################################
#PCA can use both correlation or covariance matrix,
#You tend to use the covariance matrix when the variable scales are similar and the correlation matrix when variables are on different scales.
#using covariance matrix for normalizing the data, divinding data with the maximum value of 
#of the data to bring them on same scale
#####################################################################
mnsit_train_nzv <- mnsit_train_nzv/255
covmnsit_train_nzv <- cov(mnsit_train_nzv)
princtrain <- prcomp(covmnsit_train_nzv)

#####################################################################
#Computing the eigen values and making the graph
# to know till which component most of the variance is explained
#####################################################################
#Eigen values
eig <- (princtrain$sdev)^2
# Variances in percentage
variance <- eig*100/sum(eig)
# Cumulative variances
cumvariance <- cumsum(variance)

eig.active.svm <- data.frame(eig = eig, variance = variance,
                             cumvariance = cumvariance)
str(eig.active.svm)
barplot(eig.active.svm[, 2], names.arg=1:nrow(eig.active.svm), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue",xlim = c(0,50))

#Taking a rotation of 25 as most of the variance is explained by 25 components
mnsit_train_nzv_score <- as.matrix(mnsit_train_nzv) %*% princtrain$rotation[,1:25]
mnsit_train_nzv <- cbind(label,as.data.frame(mnsit_train_nzv_score))


# Normal RBF kernel  sigma =  0.0225478857667273 cost C = 1 
model_rbf <- ksvm(label ~ ., data =mnsit_train_nzv,scale=FALSE, kernel = "rbfdot")

label_test <- as.factor(mnist_test[[1]])
mnist_test <- mnist_test[,variation[-1]]/255
# mnist_test <- scale(mnist_test[,variation[-1]])
mnist_test <- as.matrix(mnist_test) %*% princtrain$rotation[,1:25]
mnist_test <- as.data.frame(mnist_test)
mnist_test <- cbind(label_test,as.data.frame(mnist_test))


# Predicting the model results 
Eval_RBF<- predict(model_rbf, mnist_test)

#confusion matrix - RBF Kernel at C=1 and sigma =0.0223797453382114
confusionMatrix(Eval_RBF,mnist_test$label_test)
# Accuracy    : 0.973
# Sensitivity : 0.9696        
# Specificity : 0.9761 
#Normal rbf model giving good accuracy, lets check the hyperparameter
#####################################################################
#Hyperparameter tuning and Cross Validation - Non-Linear - SVM 
######################################################################

# We will use the train function from caret package to perform crossvalidation

# Making grid of "sigma" and C values. 
grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))
trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"
# Performing 5-fold cross validation over 60 percent of the train data
#as the data is very large
library(car)
trainindices= sample(1:nrow(mnsit_train_nzv), 0.6*nrow(mnsit_train_nzv))
mnsit_train_nzv_sample<- mnsit_train_nzv[trainindices,]
fit.svm_radial <- train(label~., data=mnsit_train_nzv_sample, method="svmRadial", metric=metric,
                        tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svm_radial)
# Best tune at sigma = 0.05 & C=5, Accuracy - 0.974


# Plotting model results
plot(fit.svm_radial)

#As the accuracy increased as compare to simple RBF model
#we will use this model on test data
######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm_radial, mnist_test)
confusionMatrix(evaluate_non_linear, mnist_test$label_test)
#Accuracy : .9762 =97.62 percent
#Sensitivity : .973
#Specificity : .9791
