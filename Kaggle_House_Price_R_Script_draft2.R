library(readxl)
library(mice)
library(tidyverse)
library(modelr)
library(dplyr)
library(glmnet)
library(Metrics)

#Importing the data file----------
housing_train<-read.csv("C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\train.csv")
housing_test<-read.csv("C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\test.csv")
housing_test$SalePrice<-0
housing<-rbind.data.frame(housing_train,housing_test)

view(housing_test)

str(housing_train)
summary(housing_train)
head(housing_train)
tail(housing_train)


#Train file Data Cleaning
housing_train$LotFrontage<-ifelse(is.na(housing_train$LotFrontage), mean(housing_train$LotFrontage, na.rm=TRUE), housing_train$LotFrontage)

housing_train$MSSubClass<-as.factor(housing_train$MSSubClass)
housing_train$OverallQual<-as.factor(housing_train$OverallQual)
housing_train$OverallCond<-as.factor(housing_train$OverallCond)


housing_train$Alley<-as.character(housing_train$Alley)
housing_train$Alley<-ifelse(is.na(housing_train$Alley), "None", housing_train$Alley)
housing_train$Alley<-as.factor(housing_train$Alley)


housing_train$MasVnrType<-as.character(housing_train$MasVnrType)
housing_train$MasVnrType<-ifelse(is.na(housing_train$MasVnrType), "None", housing_train$MasVnrType)
housing_train$MasVnrType<-as.factor(housing_train$MasVnrType)



housing_train$MasVnrArea<-ifelse(is.na(housing_train$MasVnrArea), mean(housing_train$MasVnrArea, na.rm=TRUE), housing_train$MasVnrArea)

housing_train$BsmtQual<-as.character(housing_train$BsmtQual)
housing_train$BsmtQual<-ifelse(is.na(housing_train$BsmtQual), "None", housing_train$BsmtQual)
housing_train$BsmtQual<-as.factor(housing_train$BsmtQual)


housing_train$BsmtCond<-as.character(housing_train$BsmtCond)
housing_train$BsmtCond<-ifelse(is.na(housing_train$BsmtCond), "None", housing_train$BsmtCond)
housing_train$BsmtCond<-as.factor(housing_train$BsmtCond)

housing_train$BsmtExposure<-as.character(housing_train$BsmtExposure)
housing_train$BsmtExposure<-ifelse(is.na(housing_train$BsmtExposure), "Gd", housing_train$BsmtExposure)
housing_train$BsmtExposure<-as.factor(housing_train$BsmtExposure)

housing_train$BsmtFinType1<-as.character(housing_train$BsmtFinType1)
housing_train$BsmtFinType1<-ifelse(is.na(housing_train$BsmtFinType1), "None", housing_train$BsmtFinType1)
housing_train$BsmtFinType1<-as.factor(housing_train$BsmtFinType1)

housing_train$BsmtFinType2<-as.character(housing_train$BsmtFinType2)
housing_train$BsmtFinType2<-ifelse(is.na(housing_train$BsmtFinType2), "None", housing_train$BsmtFinType2)
housing_train$BsmtFinType2<-as.factor(housing_train$BsmtFinType2)

housing_train$Electrical<-as.character(housing_train$Electrical)
housing_train$Electrical<-ifelse(is.na(housing_train$Electrical), "None", housing_train$Electrical)
housing_train$Electrical<-as.factor(housing_train$Electrical)

housing_train$FireplaceQu<-as.character(housing_train$FireplaceQu)
housing_train$FireplaceQu<-ifelse(is.na(housing_train$FireplaceQu), "None", housing_train$FireplaceQu)
housing_train$FireplaceQu<-as.factor(housing_train$FireplaceQu)

housing_train$GarageType<-as.character(housing_train$GarageType)
housing_train$GarageType<-ifelse(is.na(housing_train$GarageType), "None", housing_train$GarageType)
housing_train$GarageType<-as.factor(housing_train$GarageType)

housing_train$GarageYrBlt<-as.character(housing_train$GarageYrBlt)
housing_train$GarageYrBlt<-ifelse(is.na(housing_train$GarageYrBlt), "None", housing_train$GarageYrBlt)
housing_train$GarageYrBlt<-as.factor(housing_train$GarageYrBlt)


housing_train$GarageFinish<-as.character(housing_train$GarageFinish)
housing_train$GarageFinish<-ifelse(is.na(housing_train$GarageFinish), "None", housing_train$GarageFinish)
housing_train$GarageFinish<-as.factor(housing_train$GarageFinish)

housing_train$GarageQual<-as.character(housing_train$GarageQual)
housing_train$GarageQual<-ifelse(is.na(housing_train$GarageQual), "None", housing_train$GarageQual)
housing_train$GarageQual<-as.factor(housing_train$GarageQual)


housing_train$GarageCond<-as.character(housing_train$GarageCond)
housing_train$GarageCond<-ifelse(is.na(housing_train$GarageCond), "None", housing_train$GarageCond)
housing_train$GarageCond<-as.factor(housing_train$GarageCond)

housing_train$PoolQC<-as.character(housing_train$PoolQC)
housing_train$PoolQC<-ifelse(is.na(housing_train$PoolQC), "None", housing_train$PoolQC)
housing_train$PoolQC<-as.factor(housing_train$PoolQC)

housing_train$Fence<-as.character(housing_train$Fence)
housing_train$Fence<-ifelse(is.na(housing_train$Fence), "None", housing_train$Fence)
housing_train$Fence<-as.factor(housing_train$Fence)

housing_train$MiscFeature<-as.character(housing_train$MiscFeature)
housing_train$MiscFeature<-ifelse(is.na(housing_train$MiscFeature), "None", housing_train$MiscFeature)
housing_train$MiscFeature<-as.factor(housing_train$MiscFeature)


housing_train$YearBuilt<-as.factor(housing_train$YearBuilt)

housing_train$YearRemodAdd<-as.character(housing_train$YearRemodAdd)
housing_train$YearRemodAdd<-ifelse(housing_train$YearBuilt==housing_train$YearRemodAdd, "None", housing_train$YearRemodAdd)
housing_train$YearRemodAdd<-as.factor(housing_train$YearRemodAdd)

housing_train$MoSold<-as.factor(housing_train$MoSold)


housing_train$YrSold<-as.factor(housing_train$YrSold)

housing_train$MSZoning<-as.character(housing_train$MSZoning)
housing_train$MSZoning<-ifelse(housing_train$MSZoning=='C (all)',"C",housing_train$MSZoning)
housing_train$MSZoning<-as.factor(housing_train$MSZoning)

housing_train$MSSubClass<-as.integer(housing_train$MSSubClass)


#--------Test file data cleaning

str(housing_test)
summary(housing_test)
head(housing_test)
tail(housing_test)

md.pattern(housing_test)
map(housing_test, ~sum(is.na(.)))

housing_test$MSZoning<-as.character(housing_test$MSZoning)
housing_test$MSZoning<-ifelse(is.na(housing_test$MSZoning),"C" , housing_test$MSZoning)
housing_test$MSZoning<-as.factor(housing_test$MSZoning)

housing_test$MSSubClass<-as.integer(housing_test$MSSubClass)

housing_test$LotFrontage<-ifelse(is.na(housing_test$LotFrontage), mean(housing_test$LotFrontage, na.rm=TRUE), housing_test$LotFrontage)


housing_test$Alley<-as.character(housing_test$Alley)
housing_test$Alley<-ifelse(is.na(housing_test$Alley), "None", housing_test$Alley)
housing_test$Alley<-as.factor(housing_test$Alley)

housing_test$Utilities<-as.character(housing_test$Utilities)
housing_test$Utilities<-ifelse(is.na(housing_test$Utilities), "None", housing_test$Utilities)
housing_test$Utilities<-as.factor(housing_test$Utilities)

housing_test$Exterior1st<-as.character(housing_test$Exterior1st)
housing_test$Exterior1st<-ifelse(is.na(housing_test$Exterior1st), "None", housing_test$Exterior1st)
housing_test$Exterior1st<-as.factor(housing_test$Exterior1st)

housing_test$Exterior2nd<-as.character(housing_test$Exterior2nd)
housing_test$Exterior2nd<-ifelse(is.na(housing_test$Exterior2nd), "None", housing_test$Exterior2nd)
housing_test$Exterior2nd<-as.factor(housing_test$Exterior2nd)


housing_test$MasVnrType<-as.character(housing_test$MasVnrType)
housing_test$MasVnrType<-ifelse(is.na(housing_test$MasVnrType), "None", housing_test$MasVnrType)
housing_test$MasVnrType<-as.factor(housing_test$MasVnrType)


housing_test$MasVnrArea<-ifelse(is.na(housing_test$MasVnrArea), mean(housing_test$MasVnrArea, na.rm=TRUE), housing_test$MasVnrArea)

housing_test$BsmtQual<-as.character(housing_test$BsmtQual)
housing_test$BsmtQual<-ifelse(is.na(housing_test$BsmtQual), "None", housing_test$BsmtQual)
housing_test$BsmtQual<-as.factor(housing_test$BsmtQual)


housing_test$BsmtCond<-as.character(housing_test$BsmtCond)
housing_test$BsmtCond<-ifelse(is.na(housing_test$BsmtCond), "None", housing_test$BsmtCond)
housing_test$BsmtCond<-as.factor(housing_test$BsmtCond)

housing_test$BsmtExposure<-as.character(housing_test$BsmtExposure)
housing_test$BsmtExposure<-ifelse(is.na(housing_test$BsmtExposure), "Gd", housing_test$BsmtExposure)
housing_test$BsmtExposure<-as.factor(housing_test$BsmtExposure)

housing_test$BsmtFinType1<-as.character(housing_test$BsmtFinType1)
housing_test$BsmtFinType1<-ifelse(is.na(housing_test$BsmtFinType1), "None", housing_test$BsmtFinType1)
housing_test$BsmtFinType1<-as.factor(housing_test$BsmtFinType1)

housing_test$BsmtFinType2<-as.character(housing_test$BsmtFinType2)
housing_test$BsmtFinType2<-ifelse(is.na(housing_test$BsmtFinType2), "None", housing_test$BsmtFinType2)
housing_test$BsmtFinType2<-as.factor(housing_test$BsmtFinType2)

housing_test$BsmtFinSF1<-ifelse(is.na(housing_test$BsmtFinSF1), mean(housing_test$BsmtFinSF1, na.rm=TRUE), housing_test$BsmtFinSF1)
housing_test$BsmtFinSF2<-ifelse(is.na(housing_test$BsmtFinSF2), mean(housing_test$BsmtFinSF2, na.rm=TRUE), housing_test$BsmtFinSF2)
housing_test$BsmtUnfSF<-ifelse(is.na(housing_test$BsmtUnfSF), mean(housing_test$BsmtUnfSF, na.rm=TRUE), housing_test$BsmtUnfSF)
housing_test$TotalBsmtSF<-ifelse(is.na(housing_test$TotalBsmtSF), mean(housing_test$TotalBsmtSF, na.rm=TRUE), housing_test$TotalBsmtSF)
housing_test$BsmtFullBath<-ifelse(is.na(housing_test$BsmtFullBath), mean(housing_test$BsmtFullBath, na.rm=TRUE), housing_test$BsmtFullBath)
housing_test$BsmtHalfBath<-ifelse(is.na(housing_test$BsmtHalfBath), mean(housing_test$BsmtHalfBath, na.rm=TRUE), housing_test$BsmtHalfBath)
housing_test$GarageCars<-ifelse(is.na(housing_test$GarageCars), mean(housing_test$GarageCars, na.rm=TRUE), housing_test$GarageCars)
housing_test$GarageArea<-ifelse(is.na(housing_test$GarageArea), mean(housing_test$GarageArea, na.rm=TRUE), housing_test$GarageArea)



housing_test$KitchenQual<-as.character(housing_test$KitchenQual)
housing_test$KitchenQual<-ifelse(is.na(housing_test$KitchenQual), "Ex", housing_test$KitchenQual)
housing_test$KitchenQual<-as.factor(housing_test$KitchenQual)

housing_test$Functional<-as.character(housing_test$Functional)
housing_test$Functional<-ifelse(is.na(housing_test$Functional), "None", housing_test$Functional)
housing_test$Functional<-as.factor(housing_test$Functional)

housing_test$FireplaceQu<-as.character(housing_test$FireplaceQu)
housing_test$FireplaceQu<-ifelse(is.na(housing_test$FireplaceQu), "None", housing_test$FireplaceQu)
housing_test$FireplaceQu<-as.factor(housing_test$FireplaceQu)


housing_test$GarageType<-as.character(housing_test$GarageType)
housing_test$GarageType<-ifelse(is.na(housing_test$GarageType), "None", housing_test$GarageType)
housing_test$GarageType<-as.factor(housing_test$GarageType)

housing_test$GarageYrBlt<-as.character(housing_test$GarageYrBlt)
housing_test$GarageYrBlt<-ifelse(is.na(housing_test$GarageYrBlt), "None", housing_test$GarageYrBlt)
housing_test$GarageYrBlt<-as.factor(housing_test$GarageYrBlt)

housing_test$GarageFinish<-as.character(housing_test$GarageFinish)
housing_test$GarageFinish<-ifelse(is.na(housing_test$GarageFinish), "None", housing_test$GarageFinish)
housing_test$GarageFinish<-as.factor(housing_test$GarageFinish)

housing_test$GarageQual<-as.character(housing_test$GarageQual)
housing_test$GarageQual<-ifelse(is.na(housing_test$GarageQual), "None", housing_test$GarageQual)
housing_test$GarageQual<-as.factor(housing_test$GarageQual)

housing_test$GarageQual<-as.character(housing_test$GarageQual)
housing_test$GarageQual<-ifelse(is.na(housing_test$GarageQual), "None", housing_test$GarageQual)
housing_test$GarageQual<-as.factor(housing_test$GarageQual)

housing_test$PoolQC<-as.character(housing_test$PoolQC)
housing_test$PoolQC<-ifelse(is.na(housing_test$PoolQC), "None", housing_test$PoolQC)
housing_test$PoolQC<-as.factor(housing_test$PoolQC)

housing_test$Fence<-as.character(housing_test$Fence)
housing_test$Fence<-ifelse(is.na(housing_test$Fence), "None", housing_test$Fence)
housing_test$Fence<-as.factor(housing_test$Fence)

housing_test$MiscFeature<-as.character(housing_test$MiscFeature)
housing_test$MiscFeature<-ifelse(is.na(housing_test$MiscFeature), "None", housing_test$MiscFeature)
housing_test$MiscFeature<-as.factor(housing_test$MiscFeature)

housing_test$SaleType<-as.character(housing_test$SaleType)
housing_test$SaleType<-ifelse(is.na(housing_test$SaleType), "None", housing_test$SaleType)
housing_test$SaleType<-as.factor(housing_test$SaleType)

housing_test$GarageCond<-as.character(housing_test$GarageCond)
housing_test$GarageCond<-ifelse(is.na(housing_test$GarageCond), "None", housing_test$GarageCond)
housing_test$GarageCond<-as.factor(housing_test$GarageCond)

housing_test$MSZoning<-as.character(housing_test$MSZoning)
housing_test$MSZoning<-ifelse(housing_test$MSZoning=='C (all)',"C",housing_test$MSZoning)
housing_test$MSZoning<-as.factor(housing_test$MSZoning)

housing_test$OverallQual<-as.factor(housing_test$OverallQual)
housing_test$OverallCond<-as.factor(housing_test$OverallCond)

#Feature Engineering--------

md.pattern(housing_train)

housing_train$X2ndFlrSF<-ifelse(housing_train$X2ndFlrSF==0,1,housing_train$X2ndFlrSF)
housing_train$BsmtHalfBath<-ifelse(housing_train$BsmtHalfBath==0,0.01,housing_train$BsmtHalfBath)
housing_train$BsmtFullBath<-ifelse(housing_train$BsmtFullBath==0,0.01,housing_train$BsmtFullBath)
housing_train$BsmtUnfSF<-ifelse(housing_train$BsmtUnfSF==0,1,housing_train$BsmtUnfSF)
housing_train$BsmtFinSF1<-ifelse(housing_train$BsmtFinSF1==0,1,housing_train$BsmtFinSF1)
housing_train$BsmtFinSF2<-ifelse(housing_train$BsmtFinSF2==0,1,housing_train$BsmtFinSF2)
housing_train$Fireplaces<-ifelse(housing_train$Fireplaces==0,0.1,housing_train$Fireplaces)
housing_train$FullBath<-ifelse(housing_train$FullBath==0,0.1,housing_train$FullBath)
housing_train$GarageArea<-ifelse(housing_train$GarageArea==0,1,housing_train$GarageArea)
housing_train$GarageCars<-ifelse(housing_train$GarageCars==0,0.01,housing_train$GarageCars)
housing_train$HalfBath<-ifelse(housing_train$HalfBath==0,0.01,housing_train$HalfBath)
housing_train$LowQualFinSF<-ifelse(housing_train$LowQualFinSF==0,1,housing_train$LowQualFinSF)
housing_train$OpenPorchSF<-ifelse(housing_train$OpenPorchSF==0,1,housing_train$OpenPorchSF)
housing_train$ScreenPorch<-ifelse(housing_train$ScreenPorch==0,1,housing_train$ScreenPorch)
housing_train$TotalBsmtSF<-ifelse(housing_train$TotalBsmtSF==0,1,housing_train$TotalBsmtSF)
housing_train$WoodDeckSF<-ifelse(housing_train$WoodDeckSF==0,1,housing_train$WoodDeckSF)
housing_train$MasVnrArea<-ifelse(housing_train$MasVnrArea==0,1,housing_train$MasVnrArea)



housing_test$LotArea<-ifelse(housing_test$LotArea==0,1,housing_test$LotArea)
housing_test$WoodDeckSF<-ifelse(housing_test$WoodDeckSF==0,1,housing_test$WoodDeckSF)
housing_test$TotalBsmtSF<-ifelse(housing_test$TotalBsmtSF==0,1,housing_test$TotalBsmtSF)
housing_test$GrLivArea<-ifelse(housing_test$GrLivArea==0,1,housing_test$GrLivArea)
housing_test$HalfBath<-ifelse(housing_test$HalfBath==0,0.1,housing_test$HalfBath)
housing_test$GarageCars<-ifelse(housing_test$GarageCars==0,0.1,housing_test$GarageCars)
housing_test$GarageArea<-ifelse(housing_test$GarageArea==0,1,housing_test$GarageArea)
housing_test$LowQualFinSF<-ifelse(housing_test$LowQualFinSF==0,1,housing_test$LowQualFinSF)
md.pattern(housing_train)
map(housing_test, ~sum(is.na(.)))


#Train Data Set
housing_train$bathrooms <- housing_train$BsmtFullBath+ (0.5  * housing_train$BsmtHalfBath) +
        housing_train$FullBath+ (0.5*housing_train$HalfBath)
housing_train$porch <- housing_train$WoodDeckSF + housing_train$OpenPorchSF + housing_train$EnclosedPorch + 
        housing_train$X3SsnPorch + housing_train$ScreenPorch
housing_train$totalRoom <- housing_train$TotRmsAbvGrd + housing_train$KitchenAbvGr

# Test Data Set
housing_test$bathrooms <- housing_test$BsmtFullBath+ (0.5  * housing_test$BsmtHalfBath) +
        housing_test$FullBath+ (0.5*housing_test$HalfBath)
housing_test$porch <- housing_test$WoodDeckSF + housing_test$OpenPorchSF + housing_test$EnclosedPorch + 
        housing_test$X3SsnPorch + housing_test$ScreenPorch
housing_test$totalRoom <- housing_test$TotRmsAbvGrd + housing_test$KitchenAbvGr

#----Identifying and Removing Outliers


housing_train<-housing_train[-c(524,1299),]



#-------------Splitting train into train and test

train_size<-floor(0.70*nrow(housing_train))
train_size

set.seed(123)

train_ind<-sample(seq_len(nrow(housing_train)),size=train_size)

housing_train1<-housing_train[train_ind,]
housing_test1<-housing_train[-train_ind,]

#-----------------Linear Regression

reg<-lm(SalePrice~.-PoolArea-X3SsnPorch-BsmtHalfBath-LowQualFinSF-
          MiscVal-Alley-Fence-
          LotFrontage-MoSold-PoolQC-
          YrSold-Utilities-MSSubclass,housing_train1)

par(mfrow=c(1,4))
plot(reg)
summary(reg)

housing_predict<-predict(reg,housing_test1)


#---------Regression1

reg1<-step(lm(log(SalePrice)~MSZoning+log(LotArea)*log(WoodDeckSF)
         +LotShape+LotConfig+Neighborhood+Condition1+BldgType+HouseStyle
         +OverallQual+OverallCond+MasVnrType+MasVnrArea+Foundation+
        BsmtCond+BsmtExposure+BsmtFinSF2
        +BsmtUnfSF+log(TotalBsmtSF)*BsmtFinSF1+CentralAir+X1stFlrSF+X2ndFlrSF+
        log(GrLivArea)+FullBath+HalfBath+BedroomAbvGr*FullBath+KitchenAbvGr+KitchenQual
        +Fireplaces+GarageFinish+GarageCars+
        GarageArea+sqrt(OpenPorchSF)+
        ScreenPorch+SaleCondition,housing_train1),direction = "both")

summary(reg1)

housing_predict1<-exp(predict(reg1,newdata=housing_test1))




write.csv(housing_predict1, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\Predicted House Prices_reg1.csv") 


#------Regression2

reg2<-step(lm(log(SalePrice)~MSZoning+log(LotArea)*log(WoodDeckSF)*GarageArea+LotShape+	
         +LotConfig+Neighborhood+Condition1+HouseStyle+OverallQual
         +OverallCond+MasVnrType+MasVnrArea+Foundation
         +BsmtCond+BsmtExposure+BsmtFinSF2+log(TotalBsmtSF)*BsmtFinSF1+CentralAir
         +X1stFlrSF+X2ndFlrSF+log(GrLivArea)+FullBath+log(HalfBath)
         +BedroomAbvGr*FullBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+Fireplaces+FireplaceQu+GarageType
         +log(GarageCars)*log(GarageArea)+sqrt(OpenPorchSF)
         +ScreenPorch+SaleCondition+log(LowQualFinSF)+TotRmsAbvGrd,housing_train1),direction="both")

summary(reg2)

housing_predict2<-exp(predict(reg2,newdata=housing_test1))



library(ggplot2)
ggplot(housing_train1,aes(x=BsmtExposure,y=SalePrice))+geom_point()

write.csv(housing_predict2, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\Predicted House Prices_reg2.csv") 



#---------Regression 3

reg4<-step(lm(log(SalePrice)~MSZoning+log(LotArea)*log(WoodDeckSF)*GarageArea+LotShape+	
                      +LotConfig+Neighborhood+Condition1+HouseStyle+OverallQual
              +MasVnrType+MasVnrArea+Foundation
              +BsmtCond+BsmtExposure+BsmtFinSF2+log(TotalBsmtSF)*BsmtFinSF1+CentralAir
              +X1stFlrSF+X2ndFlrSF+log(GrLivArea)+FullBath
              +BedroomAbvGr+KitchenAbvGr+KitchenQual+Fireplaces+FireplaceQu+GarageType
              +log(GarageCars)*log(GarageArea)+sqrt(OpenPorchSF)
              +ScreenPorch+SaleCondition+log(LowQualFinSF)+TotRmsAbvGrd,housing_train1),direction = "both")

summary(reg4)

housing_predict4<-exp(predict(reg4,newdata=housing_test1))



library(ggplot2)
ggplot(housing_train1,aes(x=BsmtExposure,y=SalePrice))+geom_point()

write.csv(housing_predict4, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\Predicted House Prices_reg4.csv") 



#--------Regression4

reg5<-step(lm(log(SalePrice)~MSZoning*MSSubClass+log(LotArea)*log(WoodDeckSF)*GarageArea+LotShape+	
              LotConfig*LotShape+Neighborhood+Condition1+HouseStyle+OverallQual
              +MasVnrType+MasVnrArea+Foundation
              +BsmtCond+BsmtExposure+log(TotalBsmtSF)+CentralAir
              +log(X1stFlrSF)+X2ndFlrSF+log(GrLivArea)+FullBath
              +BedroomAbvGr+KitchenAbvGr+KitchenQual+Fireplaces+FireplaceQu+GarageType
              +log(GarageCars)*log(GarageArea)+sqrt(OpenPorchSF)
              +ScreenPorch+SaleCondition+log(LowQualFinSF),housing_train1),direction = "both")

summary(reg5)

housing_predict5<-exp(predict(reg5,newdata=housing_test1))



library(ggplot2)
ggplot(housing_train1,aes(x=BsmtExposure,y=SalePrice))+geom_point()

write.csv(housing_predict5, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\Predicted House Prices_reg5.csv") 



#----------reg6

reg6<-step(lm(log(SalePrice)~log(LotArea)	
         +Neighborhood+HouseStyle+OverallCond*BsmtCond+OverallQual
         +ExterQual+Foundation+Condition1	
         +log(TotalBsmtSF)+MasVnrArea	
         +log(GrLivArea)+bathrooms+totalRoom	
         +GarageCars+BldgType+
         SaleCondition+Street+LotConfig*LotShape*BldgType,data=housing_train1),direction = "both")

housing_predict6<-exp(predict(reg6,newdata=housing_test1))



write.csv(housing_predict6, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Ind Assignment 1\\Housing Price\\Predicted House Prices_reg6.csv") 
