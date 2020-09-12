churn <- read.csv("new churn.csv")
str(churn)
summary(churn)

#-------------------------

#Preprocessing
library(scales)
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
#Normalizing/Scaling columns Monthly Charges and Total Charges
#Neural Nets Normalizaing/Scaling
churn$MonthlyCharges = scales::rescale(churn$MonthlyCharges)
churn$TotalCharges = scales::rescale(churn$TotalCharges)
#Transforming MonthlyCharges by squareroot & TotalCharges by Cuberoot
#Function for cuberoot
cbrt <- function(x){
  sign(x) * abs(x)^(1/3)
}
ggplot(churn)+geom_histogram(mapping = aes(MonthlyCharges),bins = 50)
ggplot(churn)+geom_histogram(mapping = aes(TotalCharges),bins = 50)
churn$MonthlyCharges = sqrt(churn$MonthlyCharges)
churn$TotalCharges = cbrt(churn$TotalCharges)
ggplot(churn)+geom_histogram(mapping = aes(MonthlyCharges),bins = 50)
ggplot(churn)+geom_histogram(mapping = aes(TotalCharges),bins = 50)

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
churn.NN <- cbind(num,dummy)
str(churn.NN)

#-------------------------

#Oversampling
train.NN <- churn.NN[rownames(train.df),]
valid.NN <- churn.NN[rownames(valid.df),]
#Sampling
churn.sam <- rbind(train.NN,valid.NN)
train.index <- sample(c(1:dim(churn.sam)[1]),0.60*dim(churn.sam)[1])
train.sam <- churn.sam[train.index,]
valid.sam <- churn.sam[-train.index,]

#-------------------------

#Neural Nets
library(neuralnet)
#Oversampling
train.NN <- train.NN[,-1]
valid.NN <- valid.NN[,-1]
NN_model1 <- neuralnet::neuralnet(Churn~.,data = train.NN,linear.output = FALSE,hidden = 2)
plot(NN_model1,rep = "best")
valid.NN.pred1 <- as.factor(ifelse(apply(neuralnet::compute(NN_model1,valid.NN)$net.result,1,which.max)-1 == 1,"Yes","No"))
pred.prob1 <- predict(NN_model1,valid.NN,type = "response")
#Sampling
train.sam <- train.sam[,-1]
valid.sam <- valid.sam[,-1]
NN_model2 <- neuralnet::neuralnet(Churn~.,data = train.sam,linear.output = FALSE,hidden = 2)
plot(NN_model2,rep = "best")
valid.NN.pred2 <- as.factor(ifelse(apply(neuralnet::compute(NN_model2,valid.sam)$net.result,1,which.max)-1 == 1,"Yes","No"))
pred.prob2 <- predict(NN_model2,valid.sam,type = "response")

#-------------------------

#Model Performance
library(gmodels)
library(caret)
library(gains)
library(verification)
#Oversampling
#Confusion Matrix
gmodels::CrossTable(valid.NN.pred1,valid.NN$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.NN.pred1,valid.NN$Churn,positive = "Yes")
#Lift Chart
gain <- gains(ifelse(valid.NN$Churn=="Yes",1,0), pred.prob1[,2], groups=100)
plot(c(0,gain$cume.pct.of.total*sum(valid.NN$Churn=="Yes"))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.NN$Churn=="Yes"))~c(0, dim(valid.NN)[1]), lty=2)
#Decile-wise Lift Chart
heights <- gain$mean.resp/mean(ifelse(valid.NN$Churn=="Yes",1,0))
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9), 
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")
#ROC Curve
verification::roc.plot(ifelse(valid.NN$Churn=="Yes",1,0),ifelse(valid.NN.pred1=="Yes",1,0))
#Sampling
#Confusion Matrix
gmodels::CrossTable(valid.NN.pred2,valid.sam$Churn,prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(valid.NN.pred2,valid.sam$Churn,positive = "Yes")
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
verification::roc.plot(ifelse(valid.sam$Churn=="Yes",1,0),ifelse(valid.NN.pred2=="Yes",1,0))

#-------------------------



