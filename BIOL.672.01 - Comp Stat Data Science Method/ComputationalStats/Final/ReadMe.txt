
In the file "SabiqShahabWindowsFinalAssignment.R", set the wd to the location that contains the file "The_Cancer_data_1500_V2.csv"
The file 'The_Cancer_data_1500_V2.csv' contains the data used for the R script

The following plots are created in order:
ROC curve plot for Logisitic Regression
ROC curve plot for Decision Tree
ROC curve plot for Random Forest
Combined ROC curve plot for Logisitic Regression, Decision Tree, and Random Forest
Logisitic Regression Coefficients Plot
Decision Tree Visualization
Random Forest Feature Importance Plot

Written below is the figure captions written:

Figure: Comparison of the performance and feature importance of three machine learning models (Logistic Regression, Decision Tree, and Random Forest) for predicting cancer diagnosis based on patient characteristics.

The figure includes individual and combined Receiver Operating Characteristic (ROC) curves for each model, illustrating their discriminatory abilities. 
The Logistic Regression model (blue curve) achieves an AUC of 0.8966, the Decision Tree model (red curve) achieves an AUC of 0.8468, and the Random Forest model (green curve) achieves the highest AUC of 0.9317. 
The Random Forest performed the best, followed by the Linear Regression, and then the Decision Tree model

Additionally, the confusion matrices summarize each model's effectiveness in classifying cancer diagnoses. 
Logistic Regression achieved an accuracy of 82.89%, with sensitivity of 90.24% and specificity of 69.94%. 
The Decision Tree model achieved an accuracy of 84.22%, with sensitivity of 87.80% and specificity of 77.91%. 
The Random Forest model achieved the highest accuracy at 90.89%, with sensitivity of 96.52% and specificity of 80.98%.
Lower specificity suggests that the logisitc regression may have difficulty in avoiding false positives compared to other models.
The Decision Tree had a balance of sensitivity and specificity, providing predictions with a higher ability to avoid false positives compared to Logistic Regression.
Random Forest outperformed both Logistic Regression and Decision Tree in all key metrics (accuracy, sensitivity, and specificity), making it the best performing model for cancer diagnosis prediction in this case.

The Logistic Regression Coefficients plot demonstrates the relative importance of each feature in the logistic model, where higher bars indicate greater influence on the predicted outcome. 
Key features with the largest positive coefficients, suggesting a stronger association with cancer risk, include Cancer History (4.53), Gender (2.24), Smoking (2.11), Genetic Risk (1.68), and Alcohol Intake (0.70). 
Smaller coefficients include BMI (0.14), Age (0.06), and Physical Activity (-0.29), with a negative coefficient for Physical Activity suggesting a protective effect. 
The intercept is -11.25, indicating the baseline log odds of cancer diagnosis when all predictors are zero, with all coefficients statistically significant (p < 0.001).

The Decision Tree visualization displays the model's structure, with key splits based on features such as Cancer History, Genetic Risk, Smoking, Alcohol Intake, and BMI, showing how the model segments the population and the predicted diagnoses in terminal nodes.
The Decision Tree's root node shows the overall class distribution, with 65% "cancer negative" and 35% "cancer positive." 
Cancer History is the strongest predictor, with 80% of individuals with a cancer history classified as "cancer positive." 
Among those without a cancer history, Genetic Risk further divides individuals, with 45% of those with high genetic risk classified as "cancer positive." Lifestyle factors such as smoking, BMI (>30 kg/m²), and low physical activity (<150 minutes/week) also significantly influence classifications. 
Terminal nodes provide the predicted class and probability distribution, for example 90% "cancer positive" for individuals with high BMI and low physical activity.

The Random Forest feature importance plot ranks features based on mean decrease in accuracy. 
The most influential features for predicting cancer are: Cancer History (70.41), Genetic Risk (50.62), Smoking (31.78), BMI (32.67), Gender (31.81), Alcohol Intake (29.61), Age (27.67), and Physical Activity (22.61).
Higher importance values indicate greater influence on the model’s predictive performance.