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

#-------------------------

#Dummy Variable for other algorithms
#m-1 dummies
#Categorical Columns & Numerical Columns
cat <- churn[,-c(1,19,20,21)]
num <- churn[,c(1,19,20,21)]
#Function to create dummy variable
dum <- function(x){
  model.matrix(~x-1,data = churn)[,-1]
}
#Creating Dummy Variables
dummy <- data.frame(sapply(cat, dum))
#Combining variables to final dataset
churn.logit <- cbind(num,dummy)
str(churn.logit)

#-------------------------

#Oversampling
train.logit <- churn.logit[rownames(train.df),]
valid.logit <- churn.logit[rownames(valid.df),]
#Sampling
churn.sam <- rbind(train.logit,valid.logit)
train.index <- sample(c(1:dim(churn.sam)[1]),0.60*dim(churn.sam)[1])
train.sam <- churn.sam[train.index,]
valid.sam <- churn.sam[-train.index,]

#-------------------------

#Logistic Regression
#Oversampled
train.logit <- train.logit[,-1]
valid.logit <- valid.logit[,-1]
logit_model1 <- glm(Churn~.,data = train.logit,family = "binomial")
valid.logit.pred1 <- as.factor(ifelse(predict(logit_model1,valid.logit,type = "response") > 0.50,"Yes","No"))
pred.prob1 <- predict(logit_model1,valid.logit,type = "response")
#Sampled
train.sam <- train.sam[,-1]
valid.sam <- valid.sam[,-1]
logit_model2 <- glm(Churn~.,data = train.sam,family = "binomial")
valid.logit.pred2 <- as.factor(ifelse(predict(logit_model2,valid.sam,type = "response") > 0.50,"Yes","No"))
pred.prob2 <- predict(logit_model1,valid.sam,type = "response")

#-------------------------

#Model Performance
library(gmodels)
library(caret)
library(gains)
library(verification)
#Oversampled
#Confusion Matrix
gmodels::CrossTable(valid.logit.pred1,valid.logit$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.logit.pred1,valid.logit$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.logit$Churn=="Yes",1,0), pred.prob1, groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.logit$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.logit$Churn=="Yes"))~c(0, dim(valid.logit)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.logit$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.logit$Churn=="Yes",1,0),ifelse(valid.logit.pred1 == "Yes",1,0))
#Sampled
#Confusion Matrix
gmodels::CrossTable(valid.logit.pred2,valid.sam$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.logit.pred2,valid.sam$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.sam$Churn=="Yes",1,0), pred.prob2, groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.sam$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.sam$Churn=="Yes"))~c(0, dim(valid.sam)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.sam$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.sam$Churn=="Yes",1,0),ifelse(valid.logit.pred2 == "Yes",1,0))

#-------------------------
