################################# Start of Code =====================================
rm(ls())
getwd()
setwd("G:/Georgia Tech/Analytical Models/Assignments")

#install.packages("data.table")
#install.packages("kernlab")
#install.packages("caret")
#install.packages("e1071")
require(data.table)
require(kernlab)
require(caret)
require(e1071)

######################### Get data & manipulate =================================
cred_data = fread("credit_card_data.csv")

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

#The binary variables are all integers. Should convert them to factors for a binary
#response.

to_fact = function(data){
            for(i in 1:ncol(data)){
                if (length(unique(data[[i]])) == 2 & max(data[[i]]) == 1
                        & min(data[[i]]) == 0){
                  print(paste("Changing class from", class(data[[i]]),  "to factor.", sep = ""))              
                  data[[i]] = as.factor(data[[i]])
                                
                                            }
            }
          return(data)
                        }

cred_data = to_fact(cred_data)
for(i in 1:ncol(cred_data)){print(class(cred_data[[i]]))}

################################# KSVM with Cross Vaidation ========================

#Building the model

#Using the default kernel
#model_list = list()
#j = 1
for (i in c(0.1, 0.5, 1, 10, 100, 1000, 10000)){
    assign(paste("modelCV", (as.character(i)), sep = ""), 
                             ksvm(Response ~ ., data = cred_data, type='C-svc', cross=10, C=i))    
    #model_list[j] = paste("modelCV", (as.character(i)), sep = "")
    #j = j + 1
    #Making a list containing elements as models is a very bad practice.
} 

#Calculating the cross validation error
cross(modelCV0.1)
cross(modelCV0.5)
cross(modelCV1)
cross(modelCV10)
cross(modelCV100)
cross(modelCV1000)
cross(modelCV10000)


############################# Cross Validation with caret ==========================

####################################

#Good source for svm with caret
#https://www.r-bloggers.com/the-5th-tribe-support-vector-machines-and-caret/

####################################


#Define the number of folds
#This function sets the control parameters for the train function. We have different
#resampling methods in this like CV or bootstrapping etc. There are many other
#parameters, some related to the resampling and others not, that we could tune. 
numFolds = trainControl(method = "cv" , number = 10)

#Define the various complexity parameters
cGrid = expand.grid(C = c(0.1, 0.5, 1, 10, 100, 1000, 10000))

#Sigma is required for radial svm
rGrid = expand.grid(sigma = c(.01, .015, 0.2), C = c(0.1, 0.5, 1, 10, 100, 1000, 10000))

#Running the k folds cross validation on a linear svm
linear = train(Response ~ ., data = cred_data, method = "svmLinear" , trControl = numFolds, tuneGrid = cGrid)
linear

#Running the k folds cV on a radial svm
rad = train(Response ~ ., data = cred_data, method = "svmRadial" , trControl = numFolds, tuneGrid = rGrid)
rad

#Comparing the 3 models using resampling. Resamples checks if the results match
#after resampling

resamps <- resamples(list(Linear = linear, Radial = rad))
summary(resamps)

#According to the resampling, the radial kernel performs much better
#The range for the radial is more and it does have a lower minima
#compared to the linear but the maximas have a much higher value compared to linear.

#Using radial for our analysis
rad

#This also gives C = 0.5 as the best value