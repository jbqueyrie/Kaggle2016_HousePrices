#################
# 001_Load_Data #
#################

# Load each data file
dir = "C:/Users/jqu6215/Documents/Projets/Kaggle/HousePrices"
setwd(dir)

ptrain = paste(dir,"/Data/train.csv",sep="")
ptest = paste(dir,"/Data/test.csv",sep="")

train = read.csv2(ptrain, header=T,sep=",",dec=".")
test = read.csv2(ptest, header=T,sep=",",dec=".")

# Useful libraries
library(data.table)
library(testthat)
library(gridExtra)
library(corrplot)
library(GGally)
library(ggplot2)
library(e1071)
library(dplyr)

# DF exploration
names(train)
head(train)
str(train)

dim(train)

# Categorical / numeric variables
cat_var <- names(train)[which(sapply(train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
num_var <- names(train)[which(sapply(train, is.numeric))]

# Convert ID to factor
train[,1] = as.factor(train[,1])
test[,1] = as.factor(test[,1])

str(train)
str(test)

# Check missing values: global / cat_var / num_var
colSums(sapply(train, is.na))
colSums(sapply(train[,.SD,.SDcols = cat_var], is.na))
colSums(sapply(train[,.SD,.SDcols = num_var], is.na))

summary(train)

# Check log(SalePrice)
par(mfrow=c(1,2))
hist(train$SalePrice,xlab="Sale price",main="Sale prices distribution",col="blue")
hist(log(train$SalePrice),xlab="Log sale price",main="Log sale prices distribution",col="red")

par(mfrow=c(1,2))
hist(log(train$SalePrice),xlab="Log sale price",main="Log sale prices distribution",col="red")
qqnorm(log(train$SalePrice), main="Normal QQ-plot for log(SalePrice)")
qqline(log(train$SalePrice))

