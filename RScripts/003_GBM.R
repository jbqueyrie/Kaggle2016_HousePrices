###########
# 003_GBM #
###########

# Simple GBM with all other variables (no feature engineering)
train_V2 = train[,-1]
test_V2 = test[,-1]

library(gbm)
it = 5000
str(train)

# Without parameter tuning
train_gbm1 = gbm(train_V2$SalePrice~., data=train_V2, distribution="gaussian", interaction.depth = 1, 
                 shrinkage=0.05, n.trees=it,cv.folds=5,n.cores=4)

best.iter <- gbm.perf(train_gbm1,method="OOB")
print(best.iter)

predict_train_gbm1 = predict(train_gbm1,train_V2,best.iter)
print(sqrt(mean((train_V2$SalePrice-predict_train_gbm1)^2)))

predict_test_gbm1 = predict(train_gbm1,test_V2,best.iter)

sub = data.frame(test$Id,predict_test_gbm1)
names(sub)[names(sub)=="test.Id"] <- "Id"
names(sub)[names(sub)=="predict_test_gbm1"] <- "SalePrice"

# Final subset sample
write.csv2(sub,file=paste(dir,"/sub_GBM.csv",sep=""),row.names=F)