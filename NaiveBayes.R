# Installation of tidyverse and e1071 packages, and
# setting of working directory and loading tidyverse and e1071 libraries.
# install.packages("tidyverse")
# install.packages("e1071")
library(tidyverse)
library(e1071)
# setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/MIS 545/Project")

# Read telcocustomers.csv into a tibble called customerData and 
# display in console
customerData <- read_csv(file = "NaiveBayesData.csv",
                         col_types = "llllllllllllllllllllllnnl",
                         col_names = TRUE)
print(customerData)

# Display the structure and summary of customerData
str(customerData)
summary(customerData)

# DATAPREPROCESSING
# Remove columns Online Security, Online Backup, Device Protection
customerData <- customerData %>%
  select(-OnlineSecurity)
customerData <- customerData %>%
  select(-OnlineBackup)
customerData <- customerData %>%
  select(-DeviceProtection)
# Using 154 as the random seed, create a vector of 75% samples rows from the 
# customerData dataset
set.seed(154)
sampleSet <- sample(nrow(customerData), 
                    round(nrow(customerData)* 0.75),
                    replace = FALSE)

# Put the records from the 75% sample into customerDataTraining 
# and other 25 % into customerDataTesting
customerDataTraining <- customerData[sampleSet, ]
customerDataTesting  <- customerData[-sampleSet, ]

# Generate the Naive Bayes model to predict Churn 
# based on the other variables in the dataset
customerDataModel <- naiveBayes(formula = Churn ~ . ,
                                data = customerDataTraining,
                                laplace = 1)

# Build and print probabilities for each record in the testing 
# dataset and store them in churnProbability
churnProbability <- predict(customerDataModel,
                                   customerDataTesting,
                                   type = "raw")
print(churnProbability)

# Predict and display classes for each record in the testing dataset and 
# store them in churnPrediction
churnPrediction <- predict(customerDataModel,
                           customerDataTesting,
                                  type = "class")
print(churnPrediction)

# Evaluate the model by forming and displaying a confusion matrix
churnConfusionMatrix <- table(customerDataTesting$Churn,
                                     churnPrediction)
print(churnConfusionMatrix)

# Calculate false positive rate 
churnConfusionMatrix[1,2] /
  (churnConfusionMatrix[1,2] + 
     churnConfusionMatrix[1,1])

# Calculate false negative rate
churnConfusionMatrix[2,1] /
  (churnConfusionMatrix[2,1] +
     churnConfusionMatrix[2,2])

# Calculate the model predictive accuracy and store it into a variable called 
# predictiveAccuracy and display it
predictiveAccuracy <- sum(diag(churnConfusionMatrix)) / 
  nrow(customerDataTesting)
print(predictiveAccuracy)


