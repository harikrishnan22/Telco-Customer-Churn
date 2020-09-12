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

#KNN
#Dummy Variable for KNN
#m dummies
#Function to create dummy variable
dum_knn <- function(x){
  model.matrix(~x-1,data = churn)
}
#Categorical Columns & Numerical Columns
cat <- churn[,-c(1,19,20,21)]
num <- churn[,c(1,19,20,21)]
#Creating Dummy Variables
dummy <- data.frame(sapply(cat, dum_knn))
#Combining variables to final dataset
churn.knn <- cbind(num,dummy)
str(churn.knn)

#-------------------------

#Oversampling
train.knn <- churn.knn[rownames(train.df),]
valid.knn <- churn.knn[rownames(valid.df),]
#Sampling
churn.sam <- rbind(train.knn,valid.knn)
train.index <- sample(c(1:dim(churn.sam)[1]),0.60*dim(churn.sam)[1])
train.sam <- churn.sam[train.index,]
valid.sam <- churn.sam[-train.index,]

#-------------------------

#KNN
library(class)
i <- 1
error <- data.frame(matrix(ncol = 2,nrow = 0))
error_name <- c("train","valid")
colnames(error) <- error_name
while (i<=30) {
  print(i)
  knn_model1 <- class::knn(train = train.knn[,-c(1,4)],test = valid.knn[,-c(1,4)],cl = train.knn[,4],k = i)
  knn_model2 <- class::knn(train = train.knn[,-c(1,4)],test = train.knn[,-c(1,4)],cl = train.knn[,4],k = i)
  cm1 <- caret::confusionMatrix(knn_model1,valid.knn$Churn,positive = "Yes")
  cm2 <- caret::confusionMatrix(knn_model2,train.knn$Churn,positive = "Yes")
  error[i,1] <- 1 - cm2$byClass[11] 
  error[i,2] <- 1 - cm1$byClass[11]
  i = i + 1
}
#Oversampled
#K=23
knn_model1 <- class::knn(train = train.knn[,-c(1,4)],test = valid.knn[,-c(1,4)],cl = train.knn[,4],k = 23)
#Sampled
#K=23
knn_model2 <- class::knn(train = train.sam[,-c(1,4)],test = valid.sam[,-c(1,4)],cl = train.sam[,4],k = 23)

#-------------------------

#Model Performance
library(verification)
library(gmodels)
library(caret)
#Oversampled
#Confusion Matrix
gmodels::CrossTable(knn_model1,valid.knn[,4],prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(knn_model1,valid.knn$Churn,positive = "Yes")
#ROC Curve
verification::roc.plot(ifelse(valid.knn$Churn == "Yes",1,0),ifelse(knn_model1 == "Yes",1,0))
#Sampled
#Confusion Matrix
gmodels::CrossTable(knn_model2,valid.sam[,4],prop.r = FALSE,prop.c = FALSE,prop.t = FALSE,prop.chisq = FALSE)
caret::confusionMatrix(knn_model2,valid.sam$Churn,positive = "Yes")
#ROC Curve
verification::roc.plot(ifelse(valid.sam$Churn == "Yes",1,0),ifelse(knn_model2 == "Yes",1,0))

#-------------------------



