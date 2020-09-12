churn <- read.csv("new churn.csv")
str(churn)
summary(churn)

#-------------------------

#data exploration & visualization
library(ggplot2)
library(dplyr)
library(gplots)
#To detect missing values
heatmap(1*is.na(churn),Rowv = NA,Colv = NA)
#To detect missing values in corresponding rows/columns
lapply(churn,function(x) which(is.na(x)))
#Deleting observations with missing values
churn <- churn[complete.cases(churn),]
#To detect correlation among numerical variables
corr <- cor(churn[,c("tenure","MonthlyCharges","TotalCharges")])
gplots::heatmap.2(corr, Rowv = FALSE, Colv = FALSE, dendrogram = "none",cellnote = round(corr,2),notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))
#plots
ggplot2::ggplot(churn)+geom_bar(mapping = aes(x = Churn,fill = Churn))+ggtitle("Churn Count")
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y=MonthlyCharges))+ggtitle("Boxplot of Monthly Charges")
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y=TotalCharges))+ggtitle("Boxplot of Total Charges")
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y=tenure))+ggtitle("Boxplot of Tenure")
ggplot2::ggplot(churn)+geom_bar(mapping = aes(tenure,fill = tenure))+xlab("Tenure (Month)")+ggtitle("Distribution of Tenure")
g <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=gender,fill = Churn))
s <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=SeniorCitizen,fill = Churn))
p <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=Partner,fill = Churn))
d <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=Dependents,fill = Churn))
gridExtra::grid.arrange(g,s,p,d)
ps <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=PhoneService,fill = Churn))
ml <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=MultipleLines,fill = Churn))
is <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=InternetService,fill = Churn))
os <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=OnlineSecurity,fill = Churn))
gridExtra::grid.arrange(ps,ml,is,os)
ob <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=OnlineBackup,fill = Churn))
dp <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=DeviceProtection,fill = Churn))
ts <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=TechSupport,fill = Churn))
st <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=StreamingTV,fill = Churn))
gridExtra::grid.arrange(ob,dp,ts,st)
sm <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=StreamingMovies,fill = Churn))
c <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=Contract,fill = Churn))
pb <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=PaperlessBilling,fill = Churn))
pm <- ggplot2::ggplot(churn)+geom_bar(mapping = aes(x=PaymentMethod,fill = Churn))
gridExtra::grid.arrange(sm,c,pb,pm)
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = MonthlyCharges,fill = Churn))
ggplot2::ggplot(churn)+geom_boxplot(mapping = aes(y = TotalCharges,fill = Churn))
ggplot2::ggplot(churn)+geom_histogram(mapping = aes(x=MonthlyCharges))
ggplot2::ggplot(churn)+geom_histogram(mapping = aes(x=TotalCharges))

#-------------------------

#Preprocessing
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
#Visualization after preprocessing
ggplot2::ggplot(churn)+geom_bar(mapping = aes(tenure,fill = Churn))+xlab("Tenure (years)")+ggtitle("Distribution of Tenure")

#*************************
#Dummy Variable for other algorithms
#m-1 dummies
#Categorical Columns & Numerical Columns
cat <- churn[,-c(1,6,19,20)]
num <- churn[,c(1,6,19,20)]
#Function to create dummy variable
dum <- function(x){
  model.matrix(~x-1,data = churn)[,-1]
}
#Creating Dummy Variables
dummy <- data.frame(sapply(cat, dum))
#Combining variables to final dataset
churn.model <- cbind(num,dummy)
#*************************

  