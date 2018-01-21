Hrdata<-read.csv("human-resources-analytics/HR_comma_sep.csv")
View(Hrdata)
Hrdata$left<-as.factor(Hrdata$left)
Hrdata$Work_accident<-as.factor(Hrdata$Work_accident)
Hrdata$promotion_last_5years<-as.factor((Hrdata$promotion_last_5years))
str(Hrdata)

library(ROCR)
install.packages("partykit")
library(partykit)

install.packages("party")
library(party)

install.packages("rattle")
library(rattle)				

installed.packages("RColorBrewer")
library(RColorBrewer)			

install.packages("caret")
library(caret)	

set.seed(104)
#sampling
subset <- sample(nrow(Hrdata), nrow(Hrdata) * 0.7)
Hrdata.train = Hrdata[subset, ]
Hrdata.test = Hrdata[-subset, ]

Hrdata.glm0 <- glm(left ~ ., family = binomial, Hrdata.train)
summary(Hrdata.glm0)
table(Hrdata.train$left)

Hr.glm1=step(Hrdata.glm0)#all variables chosen
hist(predict(Hr.glm1, type = "response"))
a=summary(Hr.glm1)

AIC(Hr.glm1)
BIC(Hr.glm1)


a$deviance/a$df.residua
#optimal cut off prob


searchgrid = seq(0.01, 0.99, 0.01)
result = cbind(searchgrid, NA)
cost1 <- function(r, pi) {
  mean(((r == 0) & (pi > pcut)) | ((r == 1) & (pi < pcut)))
}
cost1 <- function(r, pi) {
  mean(((r == 0) & (pi > pcut)) | ((r == 1) & (pi < pcut)))
}

for (i in 1:length(searchgrid)) {
  pcut <- result[i, 1]
  result[i, 2] <- cost1(Hrdata.train$left, predict(Hr.glm1, type = "response"))
}

plot(result, ylab = "Cost in Training Set")
#optimal=0.08 weight=2

table(predict(Hr.glm1, type = "response") > 0.23)

#In sample prediction

prob.glm1.insample <- predict(Hr.glm1, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.25
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)

#confusion matrix
table(Hrdata.train$left, predicted.glm1.insample, dnn = c("Truth", "Predicted"))

#misclassification rate

mean(ifelse(Hrdata.train$left != predicted.glm1.insample, 1, 0))

library(ROCR)
#ROC in sample
predin<- prediction(prob.glm1.insample, Hrdata.train$left)
perfin <- performance(predin, "tpr", "fpr")
plot(perfin, colorize = TRUE)

#AUC in sample
AUCin=as.numeric(performance(predin,"auc")@y.values)#AUC
AUCin


###OPTIMAL CUTOFF CODE
perfspec <- performance(prediction.obj = predin, measure="spec", x.measure="cutoff")

plot(perfspec)

par(new=TRUE)

perfsens <- performance(prediction.obj = predin, measure="sens", x.measure="cutoff")

plot(perfsens)




#out of sample
prob.glm1.outsample <- predict(Hr.glm1, Hrdata.test, type = "response")
predicted.glm1.outsample <- prob.glm1.outsample > 0.25
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)

#confusion matrix outsample
table(Hrdata.test$left, predicted.glm1.outsample, dnn = c("Truth", "Predicted"))


# outsample misclassification rate
mean(ifelse(Hrdata.test$left != predicted.glm1.outsample, 1, 0))


#ROC Curve out smaple

library(ROCR)

predout <- prediction(prob.glm1.outsample, Hrdata.test$left)
perfout <- performance(predout, "tpr", "fpr")
plot(perfout, colorize = TRUE)

#AUC out sample
AUCout=as.numeric(performance(predout,"auc")@y.values)#AUC
AUCout

#CV

library(boot)
hrdata.cv <- glm(left~., family = binomial, data = Hrdata)
cv.result <- cv.glm(Hrdata, hrdata.cv, cost3, 3)
cv.result$delta #raw cross-validation estimate of prediction error, adjusted cross valid

cost3 <- function(r, pi) {
  mean(((r == 0) & (pi > 0.25)) | ((r == 1) & (pi < 0.25)))
}


#confusion matrix
table(Hrdata$left, predicted.glm1.cvsample, dnn = c("Truth", "Predicted"))

cost2 <- function(r,pi){
  logit_pred2 <- prediction (pi, r)
  logit_perf2 <- performance (logit_pred2, "tpr", "fpr")
  return(round(as.numeric(performance(logit_pred2, "auc")@y.values), 4))
}


cv.result.auc <- cv.glm(Hrdata, hrdata.cv, cost2, 3) #auc
cv.result.auc$delta

#CV SAMPLE PERFORMANCE
prob.glm1.cvsample <- predict(hrdata.cv, Hrdata, type = "response")
predicted.glm1.cvsample <- prob.glm1.cvsample > 0.4
predicted.glm1.cvsample <- as.numeric(predicted.glm1.cvsample)

#ROC
predCV<- prediction(prob.glm1.cvsample, Hrdata$left)
perfCV <- performance(predCV, "tpr", "fpr")
plot(perfCV,colorize=T)


#logs odds model

library (devtools)
library (broom)
Tidy_LogModel_a <- tidy(Hr.glm1)
Tidy_LogModel3 <- subset(Tidy_LogModel_a, term != "(Intercept)")
#all variables except intercept

#Add calculations

Tidy_LogModel3$OR <- exp(Tidy_LogModel3$estimate) #creating Odds ratio vector
Tidy_LogModel3$LL <- exp(Tidy_LogModel3$estimate - (1.96 * Tidy_LogModel3$std.error)) #Lower limit vector
Tidy_LogModel3$UL <- exp(Tidy_LogModel3$estimate + (1.96 * Tidy_LogModel3$std.error)) #Upper limit vector


#visualize to help interpretation

# creating a pointrange type plot using ggplot
library(ggplot2)

ggplot(Tidy_LogModel3, aes(x = term, y = OR, ymin = LL, ymax = UL, color = Term)) +
  geom_pointrange(aes(col = factor(term)), position=position_dodge(width=0.30)) + #main plot
  ylab("Odds ratio & 95% CI") + # y axis title
  geom_hline(aes(yintercept = 1)) + #black horizontal line at 1
  xlab("") + # blank x axis title
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotating x axis labels

### CART
set.seed(104)
subset = sample(nrow(HRData), nrow(HRData)*.8)
HRData_train = HRData[subset,]
HRData_test = HRData[-subset,]

#Regression Tree(before pruning)
#regression tree, not pruned####
install.packages("rpart")
library(rpart)
hr_rpart_1 = rpart(formula = left ~ ., method="class", data = HRData_train,
                   parms = list(loss = matrix(c(0, 10, 1, 0), nrow = 2)))
hr_rpart_1 
plot(hr_rpart_1)
text(hr_rpart_1)

#in- sample tree predictions
HRData.train.pred.tree= predict(hr_rpart_1 ,HRData_train) 


#Out- sample tree predictions
HRData.test.pred.tree= predict(hr_rpart_1 ,HRData_test) 


#pruned regression tree####
HRData.largetree<-rpart(formula =left  ~ ., data =HRData_train,cp  =0.001)
plotcp(HRData.largetree)
printcp(HRData.largetree)
HRData.prune<-prune(HRData.largetree,cp = 0.0031192)

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(HRData.largetree, tweak = 1.5)
rpart.plot(HRData.prune,box.palette ="-RdYlGn")


#in- sample prunned tree predictions
HRData.train.pred.prune<-predict(HRData.prune,HRData_train)



#out- sample prunned tree predictions
HRData.test.pred.prune<-predict(HRData.prune,HRData_test)



##In sample performance
HRData.train.pred.tree= predict(hr_rpart_1 ,HRData_train,type = "class") 
mctree.train <-table(HRData_train$left, HRData.train.pred.tree, dnn = c("True", "Predicted"))
mcrate.train <-(mctree.train[1,2]+mctree.train[2,1])/sum(mctree.train)
mis_rate.tree.insample <-cost.tree(HRData_train$left, HRData.train.pred.tree)

install.packages("ROCR")
library(ROCR)
#in sample ROC and AUC
HRData.train.tree.pred2 <-predict(hr_rpart_1,HRData_train,type = "prob")
HRData.train.tree.pred3 <-HRData.train.tree.pred2 [ ,2]
HRData.train.tree.pred4 <-prediction(HRData.train.tree.pred3,HRData_train$left)
HRData.train.tree.perf <-performance(HRData.train.tree.pred4, "tpr", "fpr")

ROC_traintree <-plot(HRData.train.tree.perf, colorize = T)
AUC_traintree <-as.numeric(performance(HRData.train.tree.pred4, "auc")@y.values)
slot(performance(HRData.train.tree.pred4, "auc"), 'y.values')

##out of sample performance
HRData.test.pred.tree= predict(hr_rpart_1 ,HRData_test,type = "class") 
mctree.test <-table(HRData_test$left, HRData.test.pred.tree, dnn = c("True", "Predicted"))
mcrate.test <-(mctree.test[1,2]+mctree.test[2,1])/sum(mctree.test)
mis_rate.tree.outsample <-cost.tree(HRData_test$left, HRData.test.pred.tree)


#OUt of sample ROC and AUC
HRData.test.tree.pred5 <-predict(hr_rpart_1,HRData_test,type = "prob")
HRData.test.tree.pred6 <-HRData.test.tree.pred5 [ ,2]
HRData.test.tree.pred7 <-prediction(HRData.test.tree.pred6,HRData_test$left)
HRData.test.tree.perf <-performance(HRData.test.tree.pred7, "tpr", "fpr")

ROC_testtree <-plot(HRData.test.tree.perf, colorize = T)
AUC_testtree <-as.numeric(performance(HRData.test.tree.pred7, "auc")@y.values)
slot(performance(HRData.test.tree.pred7, "auc"), 'y.values')


###Bagging

library(ipred)
library(ROCR)
# fit model
fitin <- bagging(left~., data=Hrdata.train,coob=T)
fitout<-bagging(left~.,data=Hrdata.test,coob=T)
# summarize the fit
summary(fitin)
fitin$left

# make predictions
predictions <- predict(fitin, type="class")
predictout<-predict(fitout,type="class")
# summarize accuracy
table(predictions, Hrdata.train$left,dnn=c("Truth","Predicted"))
table(predictout, Hrdata.test$left,dnn=c("Truth","Predicted"))

mean(ifelse(Hrdata.train$left !=predictions, 1, 0))
fitout$err

#ROC in sample
inbagprob = predict(fitin, type ='prob', newdata = Hrdata.train)
bagpred = prediction(inbagprob[,2], Hrdata.train$left)
bagperf = performance(bagpred, 'tpr','fpr')
plot(bagperf, colorize=TRUE)
aucbagin =as.numeric(performance(bagpred, 'auc')@y.values)
aucbagin
outbagprob = predict(fitout, type ='prob', newdata = Hrdata.test)
bagpredout = prediction(outbagprob[,2], Hrdata.test$left)
bagperfout = performance(bagpredout, 'tpr','fpr')
plot(bagperfout, colorize=TRUE)
aucbagout =as.numeric(performance(bagpredout, 'auc')@y.values)
aucbagout

varImp(fitin)

##Bagging caret
View(Hrdata.train)
library(caret)
bag.hr=randomForest(left~., data=Hrdata.train,mtry=9,importance=T)
