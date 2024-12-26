library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot) 
library(pROC)

#Setting seed for reproducibility
set.seed(672)  
#Setting working directory
setwd('C:/Users/Sabiq Shahab/Desktop/Comp Stat Final Assignment')
#Importing data set
Data <- read.csv('The_Cancer_Data_1500_V2.csv')
#Removing data that contains no value
Data <- na.omit(Data)
#Splitting data into Training and Testing data (70% Training 30% Testing)
TrainData <- createDataPartition(Data$Diagnosis, p = 0.7, list = FALSE)
Training <- Data[TrainData, ]
Testing <- Data[-TrainData, ]

#Creating Logistic Regression model, 
#Predicting Diagnosis using Age, Gender, BMI, Smoking, GeneticRisk, PhysicalActivity, AlcoholIntake, and CancerHistory
LRmodel <- glm(Diagnosis ~ ., data = Training, family = binomial)

#Predicting on the test set using logistic regression model
LPrediction <- predict(LRmodel, Testing, type = "response")

#Converting probabilities to binary predictions
LClass <- ifelse(LPrediction > 0.5, 1, 0)

#Evaluating the logistic regression model
LRMatrix <- confusionMatrix(as.factor(LClass), as.factor(Testing$Diagnosis))
print(LRMatrix)



#Creating decision tree model
DTmodel <- rpart(Diagnosis ~ ., data = Training, method = "class")

#Predicting on the test set using decision tree model
DTPredictions <- predict(DTmodel, Testing, type = "class")

#Evaluating the decision tree model
DTMatrix <- confusionMatrix(as.factor(DTPredictions), as.factor(Testing$Diagnosis))
print(DTMatrix)




#Making sure Diagnosis is a factor
Training$Diagnosis <- factor(Training$Diagnosis)
Testing$Diagnosis <- factor(Testing$Diagnosis)

#Creating the random forest model
RFmodel <- randomForest(Diagnosis ~ ., data = Training, importance = TRUE)

#Viewing the importance of features
print(importance(RFmodel))

#Predicting on the test set using random forest
RFPredictions <- predict(RFmodel, Testing)

#Ensuring predictions are factors with the same levels as Diagnosis
RFPredictions <- factor(RFPredictions, levels = levels(Testing$Diagnosis))

#Evaluating the random forest
RFMatrix <- confusionMatrix(RFPredictions, Testing$Diagnosis)
print(RFMatrix)



#Logistic Regression Area Under Curve
logisticROC <- roc(Testing$Diagnosis, LPrediction)
auc(logisticROC)

#Decision Tree Area Under Curve
decisionTreeProb <- predict(DTmodel, Testing, type = "prob")[,2]
decisionTreeROC <- roc(Testing$Diagnosis, decisionTreeProb)
auc(decisionTreeROC)

#Random Forest Area Under Curve
randomForestProb <- predict(RFmodel, Testing, type = "prob")[,2]
randomForestROC <- roc(Testing$Diagnosis, randomForestProb)
auc(randomForestROC)




#Logistic Regression Receiver Operating Characteristic Curve Plot
logisticROC <- roc(Testing$Diagnosis, LPrediction)
logisticPlot <- ggroc(logisticROC) +
  ggtitle("Logistic Regression ROC")

#Decision Tree Regression Receiver Operating Characteristic Curve Plot
decisionTreeProb <- predict(DTmodel, Testing, type = "prob")[,2]
decisionTreeROC <- roc(Testing$Diagnosis, decisionTreeProb)
decisionTreePlot <- ggroc(decisionTreeROC) +
  ggtitle("Decision Tree ROC")

#Random Forest Regression Receiver Operating Characteristic Curve Plot
randomForestProb <- predict(RFmodel, Testing, type = "prob")[,2]
randomForestROC <- roc(Testing$Diagnosis, randomForestProb)
randomForestPlot <- ggroc(randomForestROC) +
  ggtitle("Random Forest ROC")

#Plotting all models together in one plot
combinedROC <- ggroc(list(Logistic = logisticROC, 
                          DecisionTree = decisionTreeROC, 
                          RandomForest = randomForestROC)) +
  ggtitle("ROC Curve Comparison") +
  scale_color_manual(values = c("blue", "red", "green"))

#Displaying individual plots
print(logisticPlot)
print(decisionTreePlot)
print(randomForestPlot)

# Displaying combined plot
print(combinedROC)

#Creating a Logistic Regression Coefficients Plot
logisticCoefficients <- coef(LRmodel)
logisticCoefPlot <- ggplot(data.frame(Feature = names(logisticCoefficients), 
                                      Coefficient = logisticCoefficients), aes(x = Feature, y = Coefficient)) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Logistic Regression Coefficients") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Printing the coefficient plot
print(logisticCoefPlot)



#Plotting the decision tree
DTPLOT <- rpart.plot(DTmodel, 
                     type = 4,               # Tree type
                     extra = 104,            # Displaying additional information
                     fallen.leaves = TRUE,   # Arranging leaves at the bottom
                     main = "Decision Tree Visualization", # Title
                     cex = 0.5,              # Increasing text size slightly for tree nodes
                     cex.main = 1.15,         # Slightly larger title
                     cex.sub = 1.15,          # Larger subtitle
                     shadow.col = "gray",    # Adding shadow for readability
                     gap = 2)                # Adding gap between nodes

#Printing the Decision Tree Plot
print(DTPLOT)



#Creating Random Forest Feature Importance Plot
importanceData <- data.frame(Feature = rownames(importance(RFmodel)), 
                             Importance = importance(RFmodel)[,1])

rfImportancePlot <- ggplot(importanceData, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  ggtitle("Random Forest Feature Importance")

# Print the importance plot
print(rfImportancePlot)

#Figure captions:
# Comparison of the performance of three machine learning models (Logistic Regression, Decision Tree, and Random Forest) for predicting cancer diagnosis using a dataset of patient characteristics. 
# The figure shows individual and combined Receiver Operating Characteristic (ROC) curves for each model, illustrating their discriminatory abilities in classifying cancer diagnoses. 
# The Logistic Regression model (blue curve) achieves an AUC of 0.8966, the Decision Tree model (red curve) achieves an AUC of 0.8468, and the Random Forest model (green curve) achieves the highest AUC of 0.9317. 
# The combined plot presents all three models for easy comparison. 
# In addition to the ROC curves, the Decision Tree visualization shows the model’s structure, with key splits based on features such as Cancer History, Genetic Risk, Smoking, Alcohol Intake, and BMI. 
# The plot indicates how the model segments the population based on these attributes, with terminal nodes representing predicted diagnoses. 
# The Logistic Regression Coefficients plot displays the relative importance of each feature in the logistic model, where higher bars indicate greater influence on the predicted outcome. 
# The accuracy, sensitivity, specificity, and Kappa values for each model are summarized in the confusion matrices, showing the effectiveness of each model in classifying cancer cases accurately.

#Logistic regression coefficients for the model predicting cancer diagnosis. 
#The coefficients represent the log-odds of cancer diagnosis for each variable. 
#The variables with the largest positive coefficients, indicating the highest influence on cancer risk, are 
#CancerHistory (4.53), Gender (2.24), Smoking (2.11), GeneticRisk (1.68), and AlcoholIntake (0.70). The variables with smaller coefficients include BMI (0.14), Age (0.06), and PhysicalActivity (-0.29), 
#where a negative coefficient for PhysicalActivity suggests a protective effect. 
#The intercept is -11.25, indicating the baseline log-odds of cancer diagnosis when all predictors are zero. 
#All coefficients are statistically significant (p < 0.001).

#The root node shows the overall class distribution, with 65% of individuals classified as "cancer negative" and 35% as "cancer positive." 
#Cancer history emerges as the strongest predictor, splitting the population, with 80% of individuals with a cancer history classified as "cancer positive." 
#Among those without a cancer history, genetic risk further divides individuals, with 45% of those with high genetic risk classified as "cancer positive."
#Lifestyle factors such as smoking, BMI (>30 kg/m²), and low physical activity (<150 minutes/week) significantly influence classifications, especially among subsets without a cancer history. 
#Terminal nodes (leaf nodes) provide the predicted class and the probability distribution, with examples such as 90% "cancer positive" for high BMI and low physical activity.
#Age and gender refine risk in specific branches (e.g., older males with a BMI >30 kg/m² and low activity).

#Random forest feature importance for predicting digestive cancers. 
#The plot shows the relative importance of each feature based on the mean decrease in accuracy. 
#The features were ranked as follows (from highest to lowest importance): 
#Cancer History (70.41), Genetic Risk (50.62), Smoking (31.78), BMI (32.67), Gender (31.81), Alcohol Intake (29.61), Age (27.67), and Physical Activity (22.61). 
#The higher the importance value, the more influential the feature in the model’s predictive performance.