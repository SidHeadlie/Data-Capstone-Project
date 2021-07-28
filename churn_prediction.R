

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ROSE)) install.packages("rose", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages("pROC", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(caretEnsemble)) install.packages("caretEnsemble", repos = "http://cran.us.r-project.org")


### Libraries

library(caret) # for model building
library(ggplot2) # for visualization
library(tidyverse) # for data wrangling
library(gridExtra) # for arranging plots
library(forcats) # for modifying factor levels
library(knitr) # for rendering R markdown document
library(gbm) # to access variable importance of gbm algorithm
library(caretEnsemble) # to compare models


### Read file

#Read data file (ensure character variables are encoded as factors)
data <- read.csv("telecom_churn.csv", header = TRUE, stringsAsFactors = TRUE)

Check for missing values

head(data)

sapply(data, function(x) sum(is.na(x)))

#There are 11 missing values found in the variable TotalCharges, and all other variables have non-missing values

#Look at a summary of the data
summary(data)


#Look at top rows of the data


#The variable SeniorCitizen seems to be misformatted as a numeric variable
#Convert this variable to a factor
data$SeniorCitizen <- factor(data$SeniorCitizen)

#We have many categorical variables and a couple of numeric variables
#The two numeric variables are 
#1) Tenure (how long the customer has been with the telecommunications service provider)
#2) TotalCharges

#Split data into training and validation sets
#Use a 70%/30% split

set.seed(100) #for reproducibility
index <- createDataPartition(data$Churn, p=0.7, list=FALSE)
trainset <- data[index,]
val <- data[-index,]

#Check the distribution of churn and non-churn customers
ggplot(trainset, aes(x=Churn, fill = Churn)) + geom_bar()

#There is class imbalance where the majority class consists of non-churn customers


#Is there a correlation between tenure and total charges?
ggplot(trainset, aes(x = tenure, y = MonthlyCharges)) + geom_point(col = "blue")
#There does not seem to be any significant relationship between these two numeric variables

#I suspect that total charges may be somehow derived from a cumulative sum of monthly charges over the customer's tenure

trainset$TheoreticalTotal <- trainset$tenure*trainset$MonthlyCharges

ggplot(trainset, aes(x = TotalCharges, y = TheoreticalTotal)) + geom_point(col = "blue")

#My suspicions turn out to be correct as there is almost perfect correlation between the derived total and the TotalCharges variable

#We will therefore ignore the TotalCharges variable during model building

#drop customerID, TotalCharges and TheoreticalTotal
trainset <- trainset %>% select(-c("customerID", "TotalCharges", "TheoreticalTotal"))


#I hypothesize that tenure and contract type might have a significant impact on churn
#Perhaps customers who have a longer tenure and contract will be more loyal

#Monthly charges might also be relevant in predicting churn
#Perhaps customers who spend more money will be more inclined to stay with the telecommunications service provider

#Look at distributions of categorical predictor variables for churn vs non-churn

churn <- trainset[trainset$Churn == "Yes",]
non_churn <- trainset[trainset$Churn == "No",]

plot1 <- ggplot(data=non_churn, aes(x=gender, fill = gender)) + geom_bar() + ggtitle("Non Churn") + theme(legend.position = "none")
plot2 <- ggplot(data=churn, aes(x=gender, fill = gender)) + geom_bar() + ggtitle("Churn") + theme(legend.position = "none")
grid.arrange(plot1, plot2, ncol=2)

plot3 <- ggplot(data=non_churn, aes(x=SeniorCitizen, fill = SeniorCitizen)) + geom_bar() + ggtitle("Non Churn") + theme(legend.position = "none")
plot4 <- ggplot(data=churn, aes(x=SeniorCitizen, fill = SeniorCitizen)) + geom_bar() + ggtitle("Churn") + theme(legend.position = "none")
grid.arrange(plot3, plot4, ncol=2)

plot5 <- ggplot(data=non_churn, aes(x=Partner, fill = Partner)) + geom_bar() + ggtitle("Non Churn") + theme(legend.position = "none")
plot6 <- ggplot(data=churn, aes(x=Partner, fill = Partner)) + geom_bar() + ggtitle("Churn") + theme(legend.position = "none")
grid.arrange(plot5, plot6, ncol=2)

plot7 <- ggplot(data=non_churn, aes(x=Dependents, fill = Dependents)) + geom_bar() + ggtitle("Non Churn") + theme(legend.position = "none")
plot8 <- ggplot(data=churn, aes(x=Dependents, fill = Dependents)) + geom_bar() + ggtitle("Churn") + theme(legend.position = "none")
grid.arrange(plot7, plot8, ncol=2)

plot11 <- ggplot(data=non_churn, aes(x=InternetService, fill = InternetService)) + geom_bar() + ggtitle("Non Churn") + theme(legend.position = "none")
plot12 <- ggplot(data=churn, aes(x=InternetService, fill = InternetService)) + geom_bar() + ggtitle("Churn") + theme(legend.position = "none")
grid.arrange(plot11, plot12, ncol=2)

#Compare churn rate of customers with different tenures by whether they are a senior citizen
ggplot(data = trainset) +
  aes(x = tenure, fill = Churn) +
  geom_histogram(bin = 30, colour = "#1380A1") +
  labs(title = "Churn Rate",y = "Churn", subtitle = "Distribution By Tenure, Senior Citizen and Contract") + 
  theme_bw()+ 
  facet_grid(SeniorCitizen~Contract, scales = "free")


#Compare churn rate of customers with different monthly charges by whether they are a senior citizen
ggplot(data = trainset) +
  aes(x = MonthlyCharges, fill = Churn) +
  geom_histogram(bin = 30, colour = "#1380A1") +
  labs(title = "Churn Rate",y = "Churn", subtitle = "Distribution By Tenure, Senior Citizen and Contract") + 
  theme_bw()+ 
  facet_grid(SeniorCitizen~Contract, scales = "free")

#Prepare training scheme for repeated 10-fold cross-validation
set.seed(100)
trctrl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = "final",  classProbs = TRUE)

#Build a decision tree model on the training set
set.seed(100)
model_rpart <- train(Churn ~. , data=trainset, method="rpart", trControl=trctrl, tuneLength = 10)

#Build a random forest model on the training set
set.seed(100)
model_rf <- train(Churn ~. , data=trainset, method="rf", trControl=trctrl, verbose=FALSE, tuneLength = 10)

#Build a gradient boosting model on the training set
set.seed(100)
model_gbm <- train(Churn ~. , data=trainset, method="gbm", trControl=trctrl, verbose=FALSE, tuneLength = 10)

#Build a neural network model on the training set
set.seed(100)
model_nnet <- train(Churn ~. , data=trainset, method="nnet", trControl=trctrl, trace=FALSE, tuneLength = 10)

#Calculate variable importance for the decision tree model
varimp_rpart <- varImp(model_rpart)
plot(varimp_rpart, main="Variable Importance with Decision Tree")

#Calculate variable importance for the random forest model
varimp_rf <- varImp(model_rf)
plot(varimp_rf, main="Variable Importance with Random Forest")

#Calculate variable importance for the random forest model
varimp_gbm <- varImp(model_gbm)
plot(varimp_gbm, main="Variable Importance with Gradient Boosting")

#Calculate variable importance for the neural network model
varimp_nnet <- varImp(model_nnet)
plot(varimp_nnet, main="Variable Importance with Neural Network")

#Output model performance results on the test set for each model
pred_rpart <- predict(model_rpart, val)
confusionMatrix(reference = as.factor(val$Churn), data = pred_rpart, mode = "everything")

pred_rf <- predict(model_rf, val)
confusionMatrix(reference = as.factor(val$Churn), data = pred_rf, mode = "everything")

pred_gbm <- predict(model_gbm, val)
confusionMatrix(reference = as.factor(val$Churn), data = pred_gbm, mode = "everything")

#Final accuracy value is highest for gbm model (accuracy = 0.804)

algorithmList <- c('rpart', 'rf', 'gbm')

set.seed(100)
models <- caretList(Churn ~ ., data=trainset, trControl=trctrl, methodList=algorithmList) 
results <- resamples(models)
summary(results)

scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(results, scales=scales)

