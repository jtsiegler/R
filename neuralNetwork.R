# This code uses the telcocustomers data set to generate a neural network
# and predict if a customer will cancel their service with Telco
# Installing the tidyverse and neuralnet package
# install.packages("tidyverse")
# install.packages("neuralnet")
# Loading tidyverse and neuralnet packages
library(tidyverse)
library(neuralnet)
# Setting the Working Directory
# setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/MIS 545/Project")

# Read telcoCustomersv2.csv into a tibble customerData
customerData <- read_csv(file = "NeuralNetworkData.csv",
                         col_types = "llllllllllllllllllllllnnl",
                         col_names = TRUE)

# Display customerData on console
print(customerData)

# Display the structure of customerData
str(customerData)

# Display the summary of customerData
summary(customerData)

# DATAPREPROCESSING
# Remove columns Online Security, Online Backup, Device Protection
customerData <- customerData %>%
  select(-OnlineSecurity)
customerData <- customerData %>%
  select(-OnlineBackup)
customerData <- customerData %>%
  select(-DeviceProtection)

# Scaling monthy charges for better neural network
customerData <- customerData %>%
  mutate(MonthlyChargesScaled = (MonthlyCharges - min(MonthlyCharges)) /
           (max(MonthlyCharges) - min(MonthlyCharges)))

# Scaling total charges for better neural network
customerData <- customerData %>%
  mutate(TotalChargesScaled = (TotalCharges - min(TotalCharges)) /
           (max(TotalCharges) - min(TotalCharges)))

#Delete not scaled columns
customerData <- customerData %>%
  select(-TotalCharges)
customerData <- customerData %>%
  select(-MonthlyCharges)

# Set Random Seed
set.seed(591)

# Create a sampleSet with 75% of data
sampleSet <- sample(nrow(customerData),
                    round(nrow(customerData)*0.75),
                    replace= FALSE)

# Create training dataset
customerDataTraining <- customerData[sampleSet, ]

# Create testing dataset
customerDataTesting <- customerData[-sampleSet, ]

# Generate neural network
customerDataNeuralNet <- neuralnet(
  formula = (Churn ~ FiberOptic	+ 
             BankTransfer	+ MonthlyChargesScaled),
  data = customerDataTraining,
  hidden = 1,
  act.fct = "logistic",
  linear.output = FALSE)

# Display neural network results
print(customerDataNeuralNet)

# Visualize the neural network
plot(customerDataNeuralNet)

# Generate probabilities using customerDataNeuralNet
# on training dataset
churnProbability <- compute(customerDataNeuralNet,
                                     customerDataTesting)

# Display the probabilities on console
print(churnProbability$net.result)

# Convert the probability into 0/1 predictions
churnPrediction <-
  ifelse(churnProbability$net.result > 0.5, 1, 0)

# Display the 0/1 prediction
print(churnPrediction)

# Create a confusion matrix to evaluate the model
customerDataConfusionMatrix <- table(customerDataTesting$Churn,
                                       churnPrediction)
# Display Confusion Matrix
print(customerDataConfusionMatrix)

# Calculate model predictive accuracy
predictiveAccuracy <- sum(diag(customerDataConfusionMatrix)) /
  nrow(customerDataTesting)

# Calculate false positive rate 
customerDataConfusionMatrix[1,2] /
  (customerDataConfusionMatrix[1,2] + 
     customerDataConfusionMatrix[1,1])

# Calculate false negative rate
customerDataConfusionMatrix[2,1] /
  (customerDataConfusionMatrix[2,1] +
     customerDataConfusionMatrix[2,2])

# Display Predictive Accuracy
print(predictiveAccuracy)