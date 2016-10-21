#############
# 002_Lasso #
#############

library(MASS)
library(class)
library(e1071)
library(kernlab)
library(glmnet)
library(grplasso)
library(flare)

# Part 1: Fit LASSO to data

# Question 1
names(train_V2)
fit1=glmnet(train_V2[,1:79],train_V2$SalePrice,family="gaussian",intercept=F) 
plot(fit1)

plot(fit1,xvar="lambda")
matplot(fit1$lambda,t(coef(fit1)[-1,]),type="l",col=1:3,lty=1:3,xlab="Lambda",ylab="Coefficients")
legend("topright",legend=c("X1","X2","X3"),col=1:3,lty=1:3)
title("Lasso modÃ¨le 1")


lopt=cv.glmnet(X,Y,intercept=F)$lambda.min
coef(fit1,s=lopt)

Z=-2*X1+3*X2+rnorm(1000)
fit2=glmnet(X,Z,intercept=F)
plot(fit2)
plot(fit2,xvar="lambda")
matplot(fit2$lambda,t(coef(fit2)[-1,]),type="l",col=1:3,lty=1:3,xlab="Lambda",ylab="Coefficients")
legend("topleft",legend=c("X1","X2","X3"),col=1:3,lty=1:3)
title("Lasso modÃ¨le 2")

lopt=cv.glmnet(X,Z)$lambda.min
coef(fit2,s=lopt)

min(cv.glmnet(X,Z)$cvm)

# Question 2

X1=runif(100,0,10)
X2=runif(100,0,10)
X3=X1^2
Y=X1+0.2*X2+0.2*X3+rnorm(100)
X4=rnorm(100)
X5=X1^3
X6=X1^4
X7=X1^5
X=cbind(X1,X2,X3,X4,X5,X6,X7)
dsim=data.frame(Y,X1,X2,X3,X4,X5,X6,X7)

reg.lin=lm(Y~.,data=dsim)
summary(reg.lin)

# SÃ©lection stepwise avec critÃ¨re AIC
stepAIC(reg.lin,~.,trace=TRUE,direction=c("both"))
stepAIC(reg.lin,~.,trace=TRUE,direction=c("backward"))

# SÃ©lection stepwise avec critÃ¨re BIC
stepAIC(reg.lin,~.,trace=TRUE,direction=c("both"),k=log(100))

# SÃ©lection avec LASSO
reg.la=glmnet(X,Y)
matplot(reg.la$lambda,t(coef(reg.la)[-1,]),type="l",col=1:7,lty=1:7,xlab="Lambda",ylab="Coefficients")
legend("topleft",legend=c("X1","X2","X3","X4","X5","X6","X7"),col=1:7,lty=1:7)
title("Lasso")
lopt=cv.glmnet(X,Y)$lambda.min
coef(reg.la,s=lopt)

#SÃ©lection avec Elastic Net
reg.en=glmnet(X,Y,alpha=0.8)
lopt=cv.glmnet(X,Y,alpha=0.8)$lambda.min
coef(reg.en,s=lopt)

#SÃ©lection avec Dantzig Selector
reg.ds=slim(X,Y,method="dantzig")
reg.ds$beta
matplot(reg.ds$lambda,t(reg.ds$beta),type="l",col=1:7,lty=1:7,xlab="Lambda",ylab="Coefficients")
legend("topleft",legend=c("X1","X2","X3","X4","X5","X6","X7"),col=1:7,lty=1:7)
title("Dantzig Selector")

#SÃ©lection avec Adaptive Lasso

# On utilise des poids Ã©gaux aux inverses des coefficients de l'estimateur MCO Ã  la puissance gamma
# (poids utilisÃ©s par Zou, 2006)

gamma=0.6
w=1/(abs(reg.lin$coefficients[-1]))^gamma
reg.ala=glmnet(X,Y,penalty.factor=w)
matplot(reg.ala$lambda,t(coef(reg.ala)[-1,]),type="l",col=1:7,lty=1:7,xlab="Lambda",ylab="Coefficients")
legend("topleft",legend=c("X1","X2","X3","X4","X5","X6","X7"),col=1:7,lty=1:7)
title("Adaptive Lasso")
lopt=cv.glmnet(X,Y,penalty.factor=w)$lambda.min
coef(reg.ala,s=lopt)

# SÃ©lection avec Group Lasso

# On crÃ©e des groupes : on a envie de regrouper X1 et X3, puis X6 et X7 par exemple
# Pour cela, on crÃ©e un vecteur reprÃ©sentant les groupes 

grp=c(NA,2,3,2,4,5,6,6)

# On ajoute la colonne de 1 Ã  la matrice X pour l'intercept

X=cbind(rep(1,100),X)

# On crÃ©e un vecteur de rÃ©gularisation remis Ã  l'Ã©chelle comme dans l'exemple de la fonction grplasso

l=lambdamax(X, y=Y, index = grp, penscale = sqrt,model = LinReg()) * 0.5^(0:5)

reg.gla=grplasso(X,y=Y,index=grp,lambda=l,model=LinReg())
coef(reg.gla)
plot(reg.gla)

# Partie X

esp=read.table("Espviemult.csv",sep=";",dec=".",header=T)
summary(esp)

attach(esp)
n=nrow(esp)
indextrain=sample(1:n,round(2*n/3))
esp=esp[,-1] 
lnQI=log(QI)
lnTabac=log(Tabac)
lnGDP=log(GDP)

esp=cbind(esp,lnQI,lnTabac,lnGDP)
summary(esp)
esp.lm=lm(Espvie~.,data=esp[indextrain,]) 
mean((predict(esp.lm,esp[-indextrain,-1])-esp[-indextrain,1])^2)        
# SÃ©lection stepwise avec critÃ¨re AIC
stepAIC(esp.lm,~.,trace=TRUE,direction=c("both"))
esp.lm.selA=lm(Espvie~Alcool+GDP+ID+lnQI+lnTabac+lnGDP,data=esp[indextrain,]) 
mean((predict(esp.lm.selA,esp[-indextrain,-1])-esp[-indextrain,1])^2)  

# SÃ©lection stepwise avec critÃ¨re BIC
stepAIC(esp.lm,~.,trace=TRUE,direction=c("both"),k=log(100))
esp.lm.selB=lm(Espvie~Alcool+GDP+ID+lnQI+lnGDP,data=esp[indextrain,]) 
mean((predict(esp.lm.selB,esp[-indextrain,-1])-esp[-indextrain,1])^2)  

# SVM linÃ©aire
esp.svm.opt=tune.svm(Espvie~.,data=esp[indextrain,],cost=10^(-3:2),kernel="linear") 
esp.svm.opt
esp.svm=svm(Espvie~.,data=esp[indextrain,],cost=0.1,kernel="linear") 
mean((predict(esp.svm,esp[-indextrain,-1])-esp[-indextrain,1])^2)

# SVM non linÃ©aire
esp.svm.radopt=tune.svm(Espvie~.,data=esp[indextrain,],cost=10^(-3:2),gamma=10^(-3:2),kernel="radial") 
esp.svm.radopt
esp.svm.rad=svm(Espvie~.,data=esp[indextrain,],cost=10,gamma=0.01,kernel="radial") 
mean((predict(esp.svm.rad,esp[-indextrain,-1])-esp[-indextrain,1])^2)

# RÃ©gression linÃ©aire ridge
X=cbind(Alcool,Tabac,GDP,QI,ID,lnQI,lnTabac,lnGDP)
Xapp=X[indextrain,]
Y=Espvie[indextrain]
esp.ri=glmnet(Xapp,Y,alpha=0)
matplot(esp.ri$lambda,t(coef(esp.ri)[-1,]),type="l",col=1:8,xlab="Lambda",ylab="Coefficients")
legend("topright",legend=c("Al","Ta","GDP","QI","ID","lQI","lTa","lGDP"),col=1:8,lty=1)
title("Ridge")
lopt=cv.glmnet(Xapp,Y,alpha=0)$lambda.min
coef(esp.ri,s=lopt)
esp.ri.opt=glmnet(Xapp,Y,alpha=0,lambda=lopt)
mean((predict(esp.ri.opt,newx=X[-indextrain,])-esp[-indextrain,1])^2)

# RÃ©gression linÃ©aire lasso

esp.la=glmnet(Xapp,Y,alpha=1)
matplot(esp.la$lambda,t(coef(esp.la)[-1,]),type="l",col=1:8,xlab="Lambda",ylab="Coefficients")
legend("topright",legend=c("Al","Ta","GDP","QI","ID","lQI","lTa","lGDP"),col=1:8,lty=1)
title("Lasso")
lopt=cv.glmnet(Xapp,Y,alpha=1)$lambda.min
coef(esp.la,s=lopt)
esp.la.opt=glmnet(Xapp,Y,alpha=1,lambda=lopt)
mean((predict(esp.la.opt,newx=X[-indextrain,])-esp[-indextrain,1])^2)

# RÃ©gression linÃ©aire elastic net

esp.en=glmnet(Xapp,Y,alpha=0.5)
matplot(esp.en$lambda,t(coef(esp.en)[-1,]),type="l",lty=1,col=1:8,xlab="Lambda",ylab="Coefficients")
legend("topright",legend=c("Al","Ta","GDP","QI","ID","lQI","lTa","lGDP"),col=1:8,lty=1)
title("Elastic net")
lopt=cv.glmnet(Xapp,Y,alpha=0.5)$lambda.min
coef(esp.en,s=lopt)
esp.en.opt=glmnet(Xapp,Y,alpha=0.5,lambda=lopt)
mean((predict(esp.en.opt,newx=X[-indextrain,])-esp[-indextrain,1])^2)

# Adaptive Lasso

# On utilise des poids Ã©gaux aux inverses des coefficients de l'estimateur MCO Ã  la puissance gamma
# (poids utilisÃ©s par Zou, 2006)

gamma=0.6
w=1/(abs(esp.lm$coefficients[-1]))^gamma
esp.ala=glmnet(Xapp,Y,penalty.factor=w)
lopt=cv.glmnet(Xapp,Y,penalty.factor=w)$lambda.min
coef(esp.ala,s=lopt)
esp.ala.opt=glmnet(Xapp,Y,penalty.factor=w,lambda=lopt)
mean((predict(esp.ala.opt,newx=X[-indextrain,])-esp[-indextrain,1])^2)

######### Partie XI

Bank=read.csv2("Marketbancaire.csv",header=T)
summary(Bank)


n1=sum(Bank$y=="no")
n2=sum(Bank$y=="yes")

indextrain=c(sample(which(Bank$y=="no"),round(2*n1/3)),sample(which(Bank$y=="yes"),round(2*n2/3)));
Xtrain=Bank[indextrain,1:(length(Bank)-1)]
Ytrain=Bank[indextrain,length(Bank)]
Xval=Bank[-indextrain,1:(length(Bank)-1)]
Yval=Bank[-indextrain,length(Bank)]

library(ROCR)

Bank.svm=svm(y~.,data=Bank,kernel="linear",subset=indextrain,probability=TRUE)
Bank.pred=predict(Bank.svm,type="prob",Xval,probability=TRUE)
Bank.roc=prediction(attr(Bank.pred,"probabilities")[,2],Yval)
perf=performance(Bank.roc, "tpr","fpr")
plot(perf)

Bank.svm=svm(y~.,data=Bank,kernel="linear",subset=indextrain)
Bank.pred=predict(Bank.svm,type="prob",Xval,probability=TRUE)
table(predict(Bank.svm,type="prob",Xval),Bank$y[-indextrain])

Bank$y=as.numeric(Bank$y=="no")

Bank.grpla=grplasso(y ~ ., data = Bank[indextrain,], model = LogReg(),lambda=10)
coef(Bank.grpla)
table((predict(Bank.grpla,newdata=Xval,type="response")<=0.5),Bank$y[-indextrain])
