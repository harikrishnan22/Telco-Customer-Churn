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
#Standardizing columns Monthly Charges and Total Charges
churn[,c('tenure','MonthlyCharges','TotalCharges')] = scale(churn[,c('tenure','MonthlyCharges','TotalCharges')])
#Categorical Columns & Numerical Columns
cat <- churn[,-c(1,6,19,20,21)]
num <- churn[,c(1,6,19,20,21)]
#Converting Categorical to Numerical
cat <- data.frame(lapply(cat,as.numeric))
#Combining variables to final dataset
churn.LDA <- cbind(num,cat)
str(churn.LDA)

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

#Oversampling
train.LDA <- churn.LDA[rownames(train.df),]
valid.LDA <- churn.LDA[rownames(valid.df),]
#Sampling
churn.sam <- rbind(train.LDA,valid.LDA)
train.index <- sample(c(1:dim(churn.sam)[1]),0.60*dim(churn.sam)[1])
train.sam <- churn.sam[train.index,]
valid.sam <- churn.sam[-train.index,]

#-------------------------

#Linear Discriminant Analysis
library(MASS)
#Oversampling
train.LDA <- train.LDA[,-1]
valid.LDA <- valid.LDA[,-1]
LDA_model1 <- MASS::lda(Churn~.,data = train.LDA)
valid.LDA.pred1 <- as.factor((predict(LDA_model1,valid.LDA)$class))
pred.prob1 <- predict(LDA_model1,valid.LDA)$posterior
#Sampling
train.sam <- train.sam[,-1]
valid.sam <- valid.sam[,-1]
LDA_model2 <- MASS::lda(Churn~.,data = train.sam)
valid.LDA.pred2 <- as.factor((predict(LDA_model2,valid.sam)$class))
pred.prob2 <- predict(LDA_model2,valid.sam)$posterior

#-------------------------

#Model Performance
library(gmodels)
library(caret)
library(gains)
library(verification)
#Oversampling
#Confusion Matrix
gmodels::CrossTable(valid.LDA.pred1,valid.LDA$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.LDA.pred1,valid.LDA$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.LDA$Churn=="Yes",1,0), pred.prob1[,2], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.LDA$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.LDA$Churn=="Yes"))~c(0, dim(valid.LDA)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.LDA$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.LDA$Churn=="Yes",1,0),ifelse(valid.LDA.pred1=="Yes",1,0))
#Sampling
#Confusion Matrix
gmodels::CrossTable(valid.LDA.pred2,valid.sam$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.LDA.pred2,valid.sam$Churn,positive = "Yes")
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
verification::roc.plot(ifelse(valid.sam$Churn=="Yes",1,0),ifelse(valid.LDA.pred2=="Yes",1,0))

#-------------------------