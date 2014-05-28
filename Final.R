setwd("/Users/Swupnil/Dropbox/Documents/School/Columbia/Fall2013/Data Analysis/Final/")
load("TakeHome2013.RData")
require(ggplot2)
require(rpart)

#EXPLORATORY ANALYSIS

#histogram of response
ggplot(DFC.train, aes(x=PRICE)) + geom_histogram(binwidth=.25, colour="black", fill="red",alpha=.2)
shapiro.test(DFC.train$PRICE)

#all histograms
par(mfrow=c(2,3))
for(i in 1:13){
  hist(DFC.train[,i],xlab=paste(names(DFC.train)[i]));
}

#histogram of resolution
ggplot(DFC.train, aes(x=RES)) + geom_histogram(binwidth=100, colour="black", fill="blue",alpha=.2)

#histogram of energy
ggplot(DFC.train, aes(x=ENERGY)) + geom_histogram(binwidth=3, colour="black", fill="green",alpha=.2)

#plots against response, colored by grade
qplot(DFC.train$RES,DFC.train$PRICE,xlab="Maximum Resolution",ylab="Price", colour=DFC.train$GRADE,size=5)
qplot(DFC.train$WT,DFC.train$PRICE,xlab="Weight",ylab="Price", colour=DFC.train$GRADE,size=5)
qplot(DFC.train$EXPERT,DFC.train$PRICE,xlab="Ease of Use",ylab="Price", colour=DFC.train$GRADE,size=5)
qplot(DFC.train$TIME,DFC.train$PRICE,xlab="Recharge Time",ylab="Price", colour=DFC.train$GRADE,size=20)

#various plots of maximum resolution for professional grade cameras
index = which(DFC.train$GRADE==1)
cor(DFC.train[index,])
qplot(DFC.train$RES[index],DFC.train$ANGLE[index],xlab="Maximum Resolution",ylab="Angular Coverage", size=5,main = "Professional Grade DFCameras")
plot(DFC.train$TIME[index],DFC.train$PRICE[index])


#-------------------------------------------------------

#DATA CLEANING
clean = DFC.train

#convert appropriate variables to factors
clean$GRADE = as.factor(clean$GRADE)
clean$BRAND = as.factor(clean$BRAND)
clean$NET = as.factor(clean$NET)

#standardized version of the data
clean2 = clean
for(i in 1:13){
  if(class(clean2[,i])=="numeric")
    clean2[,i] = (clean2[,i]-mean(clean2[,i]))/sd(clean2[,i])
}

#partition into new training and validation
train.index = sample(1:200)

#standardized data
train = clean2[train.index[1:150],]
val = clean2[train.index[151:200],]

#nonstandardized data
train.non = clean[train.index[1:150],]
val.non = clean[train.index[151:200],]

#-------------------------------------------------------

#MODEL BUILDING
#all subset regression using function written by Tian (included in the end of this file)
a=leaps.lm(PRICE~GRADE+TIME+ENERGY+ANGLE+WT+RES+RESL+SEN+NET+BRAND+EXPERT+DATE, data=train)

#best models under PRESS, AIC, and BIC
a[which(a$press==min(a$press)),]
a[which(a$aic==min(a$aic)),]
a[which(a$bic==min(a$bic)),]

#evaluate validation error of best models
fitPRESS=lm(PRICE~GRADE+WT+ENERGY+RES+RESL+SEN+NET+BRAND,data=train)
mean((predict(fitPRESS,val)-(val$PRICE))^2)

fitAIC=lm(PRICE~GRADE+TIME+WT+ENERGY+RES+RESL+SEN+NET+BRAND,data=train)
mean((predict(fitAIC,val)-(val$PRICE))^2)

fitBIC=lm(PRICE~GRADE+WT+RES+RESL+SEN+NET,data=train)
mean((predict(fitBIC,val)-(val$PRICE))^2)
summary(fitBIC)

#BIC is the winner

#-------------------------------------------------------
#REGRESSION TREES

#tree using variables from the BIC model

fit2 = rpart(PRICE~GRADE+WT+RES+RESL+SEN+NET,data=train,
            method="anova",minsplit=5)
mean((predict(fit2,val)-(val$PRICE))^2)

#fit tree using same variables but on non-standardized data so we can apply to test data
fit2 = rpart(PRICE~GRADE+WT+RES+RESL+SEN+NET,data=train.non,
             method="anova",minsplit=5)

mean((predict(fit2,val.non)-(val.non$PRICE))^2)

#-------------------------------------------------------
#CREATE TEST PREDICTION

#clean test set
clean.test = DFC.train
clean.test$GRADE = as.factor(clean.test$GRADE)
clean.test$BRAND = as.factor(clean.test$BRAND)
clean.test$NET = as.factor(clean.test$NET)

#predict on test set
TEST.PRED = predict(fit2,clean.test)

#output predictions
write.table(TEST.PRED, "test.predictions.txt", sep=",", quote=F, row.names=F, col.names=F)
#-------------------------------------------------------

# All Subsets Regression function
# Code from Tian
leaps.lm<-function(formula.lm, data){
  library(leaps)
  library(nlme)
  library(DAAG)
  model.lm = lm(formula.lm, data=data, x=TRUE, y=TRUE)
  xx = model.lm$x[,-1]
  yy = model.lm$y
  
  var.names = colnames(xx)
  
  leaps.lm.temp = summary(regsubsets(x=xx, y=yy, 
                                     nbest=2^ncol(xx), nvmax=2^ncol(xx),
                                     method="exhaustive", all.best=TRUE, really.big=T))    
  
  aic.list = rep(0, nrow(leaps.lm.temp$which))
  bic.list = rep(0, nrow(leaps.lm.temp$which))
  press.list = rep(0, nrow(leaps.lm.temp$which))
  model.name = rep(0, nrow(leaps.lm.temp$which))
  models.try = leaps.lm.temp$which[,-1]
  model.size = rowSums(as.matrix(models.try))
  
  for(i in 1:length(aic.list)){
    matrix.temp = as.data.frame(cbind(yy, xx[, (1:ncol(xx))[models.try[i,]]]))
    colnames(matrix.temp)[1]<-"y"
    cur.model = lm(y~., data=matrix.temp)
    aic.list[i] = extractAIC(cur.model)[2]
    bic.list[i] = aic.list[i]-2*model.size[i]+log(nrow(xx))*model.size[i]
    press.list[i] = press(cur.model)
    model.name[i] = paste(var.names[models.try[i,]], collapse=" ")
  }
  
  
  results.leaps=data.frame(model.name, model.size , leaps.lm.temp$rss, 
                           leaps.lm.temp$rsq, leaps.lm.temp$adjr2, 
                           leaps.lm.temp$cp, aic.list, bic.list, press.list)
  colnames(results.leaps)=c("model", "size", "SSE", "r2", "adjr2", 
                            "Cp", "aic", "bic", "press")
  return(results.leaps)
}
