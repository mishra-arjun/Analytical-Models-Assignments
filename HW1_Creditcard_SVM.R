################################# Start of Code =====================================
rm(ls())
getwd()
setwd("G:/Georgia Tech/Analytical Models/Assignments")

install.packages("data.table")
install.packages("kernlab")
install.packages("caret")

library(data.table)

#fread is quicker that read.table and read_table in the readr package
cred_data = fread("credit_card_data.csv")

View(cred_data)

#Changing the name of the response variable
cred_names = colnames(cred_data)
cred_names[11] = "Response"

names(cred_data) = cred_names

#Exploring the data

summary(cred_data)
str(cred_data)
unique(cred_data$V5)

#V1, V5, V6, V8 are binary
#Converting V5 to a binary (1, 0) response as there is no loss of info in doing so

cred_data$V5[cred_data$V5 == "t"] = as.integer(1)
cred_data$V5[cred_data$V5 == "f"] = as.integer(0)

class(cred_data$V5)
#as.integer does not work here as the type of the column is still the same
cred_data$V5 = as.numeric(cred_data$V5)

#Checking the correlation between various variables
#Using this command is possible as all the variables are numeric

corr_table = as.data.frame(cor(cred_data))
View(corr_table)


########We can try PCA here???

########Standardizing the variables might not be the best option as using the
#####standardized coefficients for prediction is not a very good idea.
#####The standardization will be based on the mean and standard deviation of the
#####train data. It would also mess with the new data coming in which will not
#####be standardized . How will this transformation affect the model?????
#####If we do standardize, how will we correct for that when predicting?


#Correlation between variables is not a lot. And they are also not very highly
#correlated to the response. Except for V5.

#The binary variables are all integers. Should convert them to factors for a binary
#response.

cred_data$V1 = as.factor(cred_data$V1)
cred_data$V5 = as.factor(cred_data$V5)
cred_data$V6 = as.factor(cred_data$V6)
cred_data$V8 = as.factor(cred_data$V8)
cred_data$Response = as.factor(cred_data$Response)

#There is no intuitive way to know what the variables represent so plotting is not
#a necessary option. We can directly go ahead with the model.

#Dividing up the data

require(caret)
#Caret is a huge package and is very good for dividing the data into samples
#There is a way to divide on predictors and time series data as well
#http://topepo.github.io/caret/splitting.html

set.seed(1123)
split_index = createDataPartition(cred_data$Response, p = 0.8, list = FALSE, times = 1)

#There is also a way to bootstrap samples and CV folds - createResample and createFolds

cred_train = cred_data[split_index,]
cred_test = cred_data[-split_index,]

#Testing if the distribution of the dependant variable is the same as the main data set

mean(cred_data$Response)
mean(cred_train$Response)
mean(cred_test$Response)
#It is more or less the same.

#Building the model

library(kernlab)

#Based on the understanding of the SVM algorithm, trying out plotting variables

require(ggplot2)
ggplot(data = cred_data, aes(x = cred_data$V2, y = cred_data$V3,
                             color = cred_data$Response)) + geom_point()

ggplot(data = cred_data, aes(x = cred_data$V2, y = cred_data$V4,
                             color = cred_data$Response)) + geom_point() + geom_smooth()

#There does't look like any separator in the 2-D. That is why we will use all the
#predictor variables.

#Using the SVM algorithm
#Using the default kernel

svmodel = ksvm(Response ~ . , data = cred_train, C = 10, type = "C-svc")

#=========================================
#There are a lot of parameters in the ksvm model. The type can change this to
#a regression or novelty detection model. 
#The scaled parameter can scale all the
#numeric variables and then scale them back during the prediction.

#=========================================

#Tried the model for C ranging from 1 to 100000. Got the best accuracy on the
#test data for the particular seed at C = 10.

#Taking a detailed look at the model
attributes(svmodel)
svmodel
summary(svmodel)
str(svmodel)

#Making the predictions on the test data
predict_response = predict(svmodel, newdata = cred_test)

#Making the contingency table 
table(cred_test$Response, predict_response)

#We got an ACCURACY of 90.7% with the selected seed and C value
#ACCURACY = True Positives + True Negatives/ Total Observations

#The equation for the 10 dimensional seperator

#Intercept
b = b(svmodel)
b
#The coefficients of the equation
w <- colSums(coef(svmodel)[[1]] * cred_train[unlist(alphaindex(svmodel)),])
w
