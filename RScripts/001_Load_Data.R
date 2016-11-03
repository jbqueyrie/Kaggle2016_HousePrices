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
library(dtplyr)

# DF exploration
names(train)
head(train)
str(train)

dim(train)

# Convert ID to character
train[,1] = as.character(train[,1])
test[,1] = as.character(test[,1])

# Convert integer variables to factors
names=c('MSSubClass','OverallQual','OverallCond')
train[,names] <- lapply(train[,names] , factor)
test[,names] <- lapply(test[,names] , factor)

str(train)
str(test)

# Categorical / numeric variables
num_var <- names(train)[which(sapply(train, is.numeric))]
cat_var <- names(train)[which(sapply(train, is.factor))]


# Summary statistics for each variable
summary(train)



# Check missing values
sapply(train, function(x) sum(is.na(x)))
sapply(train, function(x) round(sum(is.na(x))/nrow(train),2))

missing_var = colnames(train)[apply(train, 2, anyNA)]
sapply(train[,missing_var], function(x) round(sum(is.na(x))/nrow(train),2))


# Check log(SalePrice) - variable to explain
par(mfrow=c(1,2))
hist(train$SalePrice,xlab="Sale price",main="Sale prices distribution",col="blue")
hist(log(train$SalePrice),xlab="Log sale price",main="Log sale prices distribution",col="red")

par(mfrow=c(1,2))
hist(log(train$SalePrice),xlab="Log sale price",main="Log sale prices distribution",col="red")
qqnorm(log(train$SalePrice), main="Normal QQ-plot for log(SalePrice)")
qqline(log(train$SalePrice)) # -> Not to confident about normality of log(SalePrice)

shapiro.test(log(train$SalePrice)) # -> Normality does not hold, p-value < 0.05 (or even 0.01 for that matter)


# Plot lots of variables
train_cat <- train[,cat_var]
train_cont <- train[,num_var]

plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


# Plots of cat_var (4 graphs per window)
for (i in seq(1,ncol(train_cat),4)) {
  if ((i+3) <= ncol(train_cat)) {
    doPlots(train_cat, fun = plotHist, ii = i:(i+3), ncol = 2)
  }
}

names(train_cont)
# Plots of num_var (4 graphs per window)
doPlots(train_cont, fun = plotDen, ii = 1:6, ncol = 2)
doPlots(train_cont, fun = plotDen, ii = 7:13, ncol = 2)
doPlots(train_cont, fun = plotHist, ii = 14:18, ncol = 2)
doPlots(train_cont, fun = plotHist, ii = 19:20, ncol = 2)
doPlots(train_cont, fun = plotHist, ii = 19:20, ncol = 2)
doPlots(train_cont, fun = plotDen, ii = 28:34, ncol = 2)
doPlots(train_cont, fun = plotHist, ii = 33, ncol = 1)



