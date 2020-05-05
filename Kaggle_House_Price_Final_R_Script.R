library(readxl)
library(glmnet)
library(readxl)
library(mice)
library(tidyverse)
library(modelr)
library(dplyr)
library(glmnet)
library(Metrics)

#Import the Train & Test DataSets
housing_train<-read.csv("C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\train.csv")
housing_test<-read.csv("C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\test.csv")

str(housing_train)
str(housing_test)
summary(housing_train)
summary(housing_test)
head(housing_train)
head(housing_test)

housing_test$SalePrice<-0

#Combining train & test datasets
housing<-rbind.data.frame(housing_train,housing_test)

str(housing)
summary(housing)
head(housing)

md.pattern(housing)
map(housing, ~sum(is.na(.)))

#Data Cleaning & Imputing
housing$MSZoning<-as.character(housing$MSZoning)
housing$MSZoning<-ifelse(is.na(housing$MSZoning),"None" , housing$MSZoning)
housing$MSZoning<-as.factor(housing$MSZoning)

housing$LotFrontage<-ifelse(is.na(housing$LotFrontage), mean(housing$LotFrontage, na.rm=TRUE), housing$LotFrontage)


housing$Alley<-as.character(housing$Alley)
housing$Alley<-ifelse(is.na(housing$Alley), "None", housing$Alley)
housing$Alley<-as.factor(housing$Alley)

housing$Utilities<-as.character(housing$Utilities)
housing$Utilities<-ifelse(is.na(housing$Utilities), "None", housing$Utilities)
housing$Utilities<-as.factor(housing$Utilities)


housing$Exterior1st<-as.character(housing$Exterior1st)
housing$Exterior1st<-ifelse(is.na(housing$Exterior1st), "None", housing$Exterior1st)
housing$Exterior1st<-as.factor(housing$Exterior1st)

housing$Exterior2nd<-as.character(housing$Exterior2nd)
housing$Exterior2nd<-ifelse(is.na(housing$Exterior2nd), "None", housing$Exterior2nd)
housing$Exterior2nd<-as.factor(housing$Exterior2nd)


housing$MasVnrType<-as.character(housing$MasVnrType)
housing$MasVnrType<-ifelse(is.na(housing$MasVnrType), "None", housing$MasVnrType)
housing$MasVnrType<-as.factor(housing$MasVnrType)

housing$MasVnrArea<-ifelse(is.na(housing$MasVnrArea), mean(housing$MasVnrArea, na.rm=TRUE), housing$MasVnrArea)

housing$BsmtQual<-as.character(housing$BsmtQual)
housing$BsmtQual<-ifelse(is.na(housing$BsmtQual), "None", housing$BsmtQual)
housing$BsmtQual<-as.factor(housing$BsmtQual)


housing$BsmtCond<-as.character(housing$BsmtCond)
housing$BsmtCond<-ifelse(is.na(housing$BsmtCond), "None", housing$BsmtCond)
housing$BsmtCond<-as.factor(housing$BsmtCond)

housing$BsmtExposure<-as.character(housing$BsmtExposure)
housing$BsmtExposure<-ifelse(is.na(housing$BsmtExposure), "None", housing$BsmtExposure)
housing$BsmtExposure<-as.factor(housing$BsmtExposure)

housing$BsmtFinType1<-as.character(housing$BsmtFinType1)
housing$BsmtFinType1<-ifelse(is.na(housing$BsmtFinType1), "None", housing$BsmtFinType1)
housing$BsmtFinType1<-as.factor(housing$BsmtFinType1)

housing$BsmtFinType2<-as.character(housing$BsmtFinType2)
housing$BsmtFinType2<-ifelse(is.na(housing$BsmtFinType2), "None", housing$BsmtFinType2)
housing$BsmtFinType2<-as.factor(housing$BsmtFinType2)


housing$BsmtFinSF1<-ifelse(is.na(housing$BsmtFinSF1), mean(housing$BsmtFinSF1, na.rm=TRUE), housing$BsmtFinSF1)
housing$BsmtFinSF2<-ifelse(is.na(housing$BsmtFinSF2), mean(housing$BsmtFinSF2, na.rm=TRUE), housing$BsmtFinSF2)
housing$BsmtUnfSF<-ifelse(is.na(housing$BsmtUnfSF), mean(housing$BsmtUnfSF, na.rm=TRUE), housing$BsmtUnfSF)
housing$TotalBsmtSF<-ifelse(is.na(housing$TotalBsmtSF), mean(housing$TotalBsmtSF, na.rm=TRUE), housing$TotalBsmtSF)


housing$Electrical<-as.character(housing$Electrical)
housing$Electrical<-ifelse(is.na(housing$Electrical), "None", housing$Electrical)
housing$Electrical<-as.factor(housing$Electrical)

housing$BsmtFullBath<-ifelse(is.na(housing$BsmtFullBath), mean(housing$BsmtFullBath, na.rm=TRUE), housing$BsmtFullBath)
housing$BsmtHalfBath<-ifelse(is.na(housing$BsmtHalfBath), mean(housing$BsmtHalfBath, na.rm=TRUE), housing$BsmtHalfBath)


housing$KitchenQual<-as.character(housing$KitchenQual)
housing$KitchenQual<-ifelse(is.na(housing$KitchenQual), "Ex", housing$KitchenQual)
housing$KitchenQual<-as.factor(housing$KitchenQual)

housing$Functional<-as.character(housing$Functional)
housing$Functional<-ifelse(is.na(housing$Functional), "None", housing$Functional)
housing$Functional<-as.factor(housing$Functional)

housing$FireplaceQu<-as.character(housing$FireplaceQu)
housing$FireplaceQu<-ifelse(is.na(housing$FireplaceQu), "None", housing$FireplaceQu)
housing$FireplaceQu<-as.factor(housing$FireplaceQu)

housing$GarageType<-as.character(housing$GarageType)
housing$GarageType<-ifelse(is.na(housing$GarageType), "None", housing$GarageType)
housing$GarageType<-as.factor(housing$GarageType)

housing$GarageYrBlt<-as.character(housing$GarageYrBlt)
housing$GarageYrBlt<-ifelse(is.na(housing$GarageYrBlt), "None", housing$GarageYrBlt)
housing$GarageYrBlt<-as.factor(housing$GarageYrBlt)

housing$GarageFinish<-as.character(housing$GarageFinish)
housing$GarageFinish<-ifelse(is.na(housing$GarageFinish), "None", housing$GarageFinish)
housing$GarageFinish<-as.factor(housing$GarageFinish)


housing$GarageCars<-ifelse(is.na(housing$GarageCars), mean(housing$GarageCars, na.rm=TRUE), housing$GarageCars)
housing$GarageArea<-ifelse(is.na(housing$GarageArea), mean(housing$GarageArea, na.rm=TRUE), housing$GarageArea)

housing$GarageCond<-as.character(housing$GarageCond)
housing$GarageCond<-ifelse(is.na(housing$GarageCond), "None", housing$GarageCond)
housing$GarageCond<-as.factor(housing$GarageCond)

housing$GarageQual<-as.character(housing$GarageQual)
housing$GarageQual<-ifelse(is.na(housing$GarageQual), "None", housing$GarageQual)
housing$GarageQual<-as.factor(housing$GarageQual)

housing$PoolQC<-as.character(housing$PoolQC)
housing$PoolQC<-ifelse(is.na(housing$PoolQC), "None", housing$PoolQC)
housing$PoolQC<-as.factor(housing$PoolQC)

housing$Fence<-as.character(housing$Fence)
housing$Fence<-ifelse(is.na(housing$Fence), "None", housing$Fence)
housing$Fence<-as.factor(housing$Fence)

housing$MiscFeature<-as.character(housing$MiscFeature)
housing$MiscFeature<-ifelse(is.na(housing$MiscFeature), "None", housing$MiscFeature)
housing$MiscFeature<-as.factor(housing$MiscFeature)

housing$SaleType<-as.character(housing$SaleType)
housing$SaleType<-ifelse(is.na(housing$SaleType), "None", housing$SaleType)
housing$SaleType<-as.factor(housing$SaleType)




#----Split Train & test

housing.train<-filter(housing,Id<=1050)
housing.test<-filter(housing,Id>=1051 & Id<=1460)
housing.prediction<-filter(housing,Id>=1461)



#-----Lasso Model 1
y<-log(housing.train$SalePrice)
X<-model.matrix(Id~MSSubClass+MSZoning+LotFrontage+log(LotArea)+Street+Alley+LotShape+LandContour	
                +Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType	
                +HouseStyle+OverallQual+OverallCond+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl	
                +Exterior1st+Exterior2nd+MasVnrType+MasVnrArea+ExterQual+ExterCond+Foundation+BsmtQual
                +BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinType2+BsmtFinSF2+BsmtUnfSF
                +TotalBsmtSF+Heating+HeatingQC+CentralAir+Electrical+X1stFlrSF+X2ndFlrSF+LowQualFinSF	
                +log(GrLivArea)+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+KitchenQual	
                +TotRmsAbvGrd+Functional+Fireplaces+FireplaceQu+GarageType+GarageYrBlt+GarageFinish	
                +GarageCars+GarageArea+GarageQual+GarageCond+PavedDrive+WoodDeckSF+OpenPorchSF+EnclosedPorch	
                +X3SsnPorch+ScreenPorch+PoolArea+PoolQC+Fence+MiscFeature+MiscVal+MoSold+YrSold+SaleType
                +SaleCondition,housing)[,-1]
X<-cbind(housing$Id,X)

nrow(X)
nrow(housing)

# split X into testing, trainig/holdout and prediction 
X.training<-subset(X,X[,1]<=1050)
X.testing<-subset(X, (X[,1]>=1051 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>=1461)

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) 
plot(crossval)
penalty.lasso <- crossval$lambda.min 
log(penalty.lasso) 
plot(crossval,xlim=c(-8.5,-6),ylim=c(0.006,0.008)) 
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso)
coef(lasso.opt.fit) 

lasso.prediction <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))

write.csv(lasso.prediction, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\Predicted House Prices_lasso.csv") 




#-----Lasso Final Model after removing some insgnificant variable decided by analyzing Lasso Co-efficient 
y<-log(housing.train$SalePrice)
X<-model.matrix(Id~MSZoning+LotFrontage+log(LotArea)+Street+LotShape+	
                  Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType	
                +OverallQual+OverallCond+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl	
                +Exterior1st+MasVnrArea+ExterQual+ExterCond+Foundation+BsmtQual
                +BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinType2+
                  TotalBsmtSF+Heating+HeatingQC+CentralAir+LowQualFinSF	
                +log(GrLivArea)+BsmtFullBath+FullBath+HalfBath+KitchenAbvGr+KitchenQual	
                +TotRmsAbvGrd+Functional+Fireplaces+FireplaceQu+GarageType+GarageYrBlt+GarageFinish	
                +GarageCars+GarageArea+PavedDrive+WoodDeckSF+	
                +ScreenPorch+PoolArea+Fence+MiscFeature+MiscVal+MoSold+YrSold+SaleType
                +SaleCondition,housing)[,-1]
X<-cbind(housing$Id,X)

nrow(X)
nrow(housing)

# split X into testing, trainig/holdout and prediction 
X.training<-subset(X,X[,1]<=1050)
X.testing<-subset(X, (X[,1]>=1051 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>=1461)

#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) 
plot(crossval)
penalty.lasso <- crossval$lambda.min 
log(penalty.lasso) 
plot(crossval,xlim=c(-8.5,-6),ylim=c(0.006,0.008)) 
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso)
coef(lasso.opt.fit) 

lasso.prediction <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))

write.csv(lasso.prediction, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\Predicted House Prices_lasso1.csv") 




