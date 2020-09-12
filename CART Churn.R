churn <- read.csv("new churn.csv")
str(churn)
summary(churn)

#-------------------------

#Preprocessing
#Deleting observations with missing values
churn <- churn[complete.cases(churn),]
#Function to change 'No Phone/Internet Service to No'
sub1 <- function(x){
  gsub("No phone service","No",x)
}
sub2 <- function(x){
  gsub("No internet service","No",x)
}
#Applying function sub to data frame
churn <- data.frame(lapply(churn, sub1))
churn <- data.frame(lapply(churn, sub2))
#Converting factor to numeric
churn$tenure <- as.numeric(as.character(churn$tenure))
churn$MonthlyCharges <- as.numeric(as.character(churn$MonthlyCharges))
churn$TotalCharges <- as.numeric(as.character(churn$TotalCharges))
#Function to convert months to years
conv <- function(x){
  x/12
}
churn$tenure <- sapply(churn$tenure,conv)
#Binning tenure
churn$tenure[churn$tenure >= 0 & churn$tenure <=1] = '0-1'
churn$tenure[churn$tenure > 1 & churn$tenure <=2] = '1-2'
churn$tenure[churn$tenure > 2 & churn$tenure <=3] = '2-3'
churn$tenure[churn$tenure > 3 & churn$tenure <=4] = '3-4'
churn$tenure[churn$tenure > 4 & churn$tenure <=5] = '4-5'
churn$tenure[churn$tenure > 5 & churn$tenure <=6] = '5-6'
churn$tenure <- as.factor(churn$tenure)
#Standardizing columns Monthly Charges and Total Charges
churn[,c('MonthlyCharges','TotalCharges')] = scale(churn[,c('MonthlyCharges','TotalCharges')])
churn$Churn <- as.factor(churn$Churn)

#-------------------------

#Partioning Data
#Original ratio
set.seed(123)
or <- sum(churn$Churn == "Yes")/sum(churn$Churn == "No")
churn.yes.index <- churn$Churn == "Yes"
churn.no.index <- churn$Churn == "No"
churn.yes.df <- churn[churn.yes.index,]
churn.no.df <- churn[churn.no.index,]
#Training/Validation
#Yes
train.yes.index <- sample(c(1:dim(churn.yes.df)[1]),dim(churn.yes.df)[1]/2)
train.yes.df <- churn.yes.df[train.yes.index,]
valid.yes.df <- churn.yes.df[-train.yes.index,]
#No
train.no.index <- sample(c(1:dim(churn.no.df)[1]),dim(churn.yes.df)[1]/2)
train.no.df <- churn.no.df[train.no.index,]
valid.no.df <- churn.no.df[-train.no.index,]
valid.no.index <- sample(c(1:dim(valid.no.df)[1]),(dim(train.yes.df)[1]/or))
valid.no.df <- churn.no.df[valid.no.index,]
#Combining Train/Valid
train.df <- rbind(train.yes.df,train.no.df)
valid.df <- rbind(valid.yes.df,valid.no.df)
#Oversampling
train.CT <- train.df
valid.CT <- valid.df
#Sampling
churn.sam <- rbind(train.CT,valid.CT)
train.index <- sample(c(1:dim(churn.sam)[1]),0.60*dim(churn.sam)[1])
train.sam <- churn.sam[train.index,]
valid.sam <- churn.sam[-train.index,]

#-------------------------

#Classification Tree
library(rpart)
library(rpart.plot)
#Oversampled
train.CT <- train.CT[,-1]
valid.CT <- valid.CT[,-1]
CT_model1 <- rpart::rpart(Churn~.,data = train.CT,method = "class")
rpart.plot::prp(CT_model1,type = 1, extra = 1, split.font = 1, varlen = -10,under = TRUE)
valid.CT.pred1 <- as.factor(predict(CT_model1,valid.CT,type = "class"))
pred.prob1 <- predict(CT_model1,valid.CT,type = "prob")
#Sampled
train.sam <- train.sam[,-1]
valid.sam <- valid.sam[,-1]
CT_model2 <- rpart::rpart(Churn~.,data = train.sam,method = "class")
rpart.plot::prp(CT_model2,type = 1, extra = 1, split.font = 1, varlen = -10,under = TRUE)
valid.CT.pred2 <- as.factor(predict(CT_model2,valid.sam,type = "class"))
pred.prob2 <- predict(CT_model2,valid.sam,type = "prob")

#-------------------------

#Model Performance
library(gmodels)
library(caret)
library(gains)
library(verification)
#Oversampled
#Confusion Matrix
gmodels::CrossTable(valid.CT.pred1,valid.CT$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.CT.pred1,valid.CT$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.CT$Churn=="Yes",1,0), pred.prob1[,2], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.CT$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.CT$Churn=="Yes"))~c(0, dim(valid.CT)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.CT$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.CT$Churn=="Yes",1,0),ifelse(valid.CT.pred1 == "Yes",1,0))
#Sampled
#Confusion Matrix
gmodels::CrossTable(valid.CT.pred2,valid.sam$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.CT.pred2,valid.sam$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.sam$Churn=="Yes",1,0), pred.prob2[,2], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.sam$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.sam$Churn=="Yes"))~c(0, dim(valid.sam)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.sam$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.sam$Churn=="Yes",1,0),ifelse(valid.CT.pred2 == "Yes",1,0))

#-------------------------

#Pruning Classification Tree
#Oversampled
CT_pruned1 <- rpart::prune(CT_model1, cp = CT_model1$cptable[which.min(CT_model1$cptable[,"xerror"]),"CP"])
rpart.plot::prp(CT_pruned1,type = 1, extra = 1, split.font = 1, varlen = -10,under = TRUE)
valid.CT.pred1 <- as.factor(predict(CT_pruned1,valid.CT,type = "class"))
pred.prob1 <- predict(CT_pruned1,valid.CT,type = "prob")
#Sampled
CT_pruned2 <- rpart::prune(CT_model2, cp = CT_model2$cptable[which.min(CT_model2$cptable[,"xerror"]),"CP"])
rpart.plot::prp(CT_pruned2,type = 1, extra = 1, split.font = 1, varlen = -10,under = TRUE)
valid.CT.pred2 <- as.factor(predict(CT_pruned2,valid.sam,type = "class"))
pred.prob2 <- predict(CT_pruned2,valid.sam,type = "prob")

#-------------------------

#Model Performance
library(gmodels)
library(caret)
library(gains)
library(verification)
#Oversampled
#Confusion Matrix
gmodels::CrossTable(valid.CT.pred1,valid.CT$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.CT.pred1,valid.CT$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.CT$Churn=="Yes",1,0), pred.prob1[,2], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.CT$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.CT$Churn=="Yes"))~c(0, dim(valid.CT)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.CT$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.CT$Churn=="Yes",1,0),ifelse(valid.CT.pred1 == "Yes",1,0))
#Sampled
#Confusion Matrix
gmodels::CrossTable(valid.CT.pred2,valid.sam$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.CT.pred2,valid.sam$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.sam$Churn=="Yes",1,0), pred.prob2[,2], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.sam$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.sam$Churn=="Yes"))~c(0, dim(valid.sam)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.sam$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.sam$Churn=="Yes",1,0),ifelse(valid.CT.pred2 == "Yes",1,0))

#-------------------------

#Random Forest
library(randomForest)
#Oversampled
RF_model1 <- randomForest::randomForest(Churn ~ ., data = train.CT, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)  
#Variable Importance Plot
varImpPlot(RF_model1, type = 1)
valid.CT.pred1 <- as.factor(predict(RF_model1,valid.CT,type = "class"))
pred.prob1 <- predict(RF_model1,valid.CT,type = "prob")
#Sampled
RF_model2 <- randomForest::randomForest(Churn ~ ., data = train.sam, ntree = 500, mtry = 4, nodesize = 5, importance = TRUE)  
#Variable Importance Plot
varImpPlot(RF_model2, type = 1)
valid.CT.pred2 <- as.factor(predict(RF_model2,valid.sam,type = "class"))
pred.prob2 <- predict(RF_model2,valid.sam,type = "prob")

#-------------------------

#Model Performance
library(gmodels)
library(caret)
library(gains)
library(verification)
#Oversampled
#Confusion Matrix
gmodels::CrossTable(valid.CT.pred1,valid.CT$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.CT.pred1,valid.CT$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.CT$Churn=="Yes",1,0), pred.prob1[,2], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.CT$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.CT$Churn=="Yes"))~c(0, dim(valid.CT)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.CT$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.CT$Churn=="Yes",1,0),ifelse(valid.CT.pred1 == "Yes",1,0))
#Sampled
#Confusion Matrix
gmodels::CrossTable(valid.CT.pred2,valid.sam$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.CT.pred2,valid.sam$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.sam$Churn=="Yes",1,0), pred.prob2[,2], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.sam$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.sam$Churn=="Yes"))~c(0, dim(valid.sam)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.sam$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.sam$Churn=="Yes",1,0),ifelse(valid.CT.pred2 == "Yes",1,0))

#-------------------------

#Boosted Trees
library(adabag)
#Oversampled
boost_model1 <- adabag::boosting(Churn~.,data = train.CT)
valid.CT.pred1 <- as.factor(predict(boost_model1,valid.CT)$class)
pred.prob1 <- predict(boost_model1,valid.CT)$prob
#Sampled
boost_model2 <- adabag::boosting(Churn~.,data = train.sam)
valid.CT.pred2 <- as.factor(predict(boost_model2,valid.sam)$class)
pred.prob2 <- predict(boost_model2,valid.sam)$prob

#-------------------------

#Model Performance
library(gmodels)
library(caret)
library(gains)
library(verification)
#Oversampled
#Confusion Matrix
gmodels::CrossTable(valid.CT.pred1,valid.CT$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.CT.pred1,valid.CT$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.CT$Churn=="Yes",1,0), pred.prob1[,2], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.CT$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.CT$Churn=="Yes"))~c(0, dim(valid.CT)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.CT$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.CT$Churn=="Yes",1,0),ifelse(valid.CT.pred1 == "Yes",1,0))
#Sampled
#Confusion Matrix
gmodels::CrossTable(valid.CT.pred2,valid.sam$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.CT.pred2,valid.sam$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.sam$Churn=="Yes",1,0), pred.prob2[,2], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.sam$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.sam$Churn=="Yes"))~c(0, dim(valid.sam)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.sam$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.sam$Churn=="Yes",1,0),ifelse(valid.CT.pred2 == "Yes",1,0))

#-------------------------