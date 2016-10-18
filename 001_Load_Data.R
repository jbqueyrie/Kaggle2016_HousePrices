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

dt.out = dt[, list(v1 = sum(v1),  lapply(.SD,mean)), by = grp, .SDcols = sd.cols]

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


# Plot lots of variables
train_cat <- train[,.SD, .SDcols = cat_var]
train_cont <- train[,.SD,.SDcols = num_var]

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}


# Simple GBM with all other variables (no feature engineering)
train_V2 = train[,-1]
test_V2 = test[,-1]

library(gbm)
it = 5000

train_gbm1 = gbm(train_V2$SalePrice~., data=train_V2, distribution="gaussian", interaction.depth = 1, 
             shrinkage=0.05, n.trees=it,cv.folds=5,n.cores=4)

best.iter <- gbm.perf(train_gbm1,method="OOB")
print(best.iter)

predict_train_gbm1 = predict(train_gbm1,train_V2,best.iter)
print(sqrt(mean((train_V2$SalePrice-predict_train_gbm1)^2)))

predict_test_gbm1 = predict(train_gbm1,test_V2,best.iter)

