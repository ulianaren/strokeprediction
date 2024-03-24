setwd("C:\\r_final")
library(readr)
data <- read_csv("healthcare-dataset-stroke-data.csv")

View(data)

#remove id column as it won't be useful


install.packages("dplyr")
library(dplyr)
data <- data %>%
  select(-id)

##############Clean data a bit. 

### We have N/A values in bmi column, let's remove them 

library(dplyr)
data <- data %>%
  mutate(bmi = ifelse(bmi == "N/A", NA, as.numeric(bmi))) %>%
  filter(!is.na(bmi))

unique(data$bmi)

###we also have Unknown in Smoke column, let's remove

data <- data[data$smoking_status != 'Unknown', ] 

#we see that in age some of the numbers are not integers, we will round them to nearest integer 
data$age <- round(data$age)

###categorical variables encoding
unique(data$gender)
unique(data$smoking_status)
unique(data$ever_married)
unique(data$work_type)
unique(data$Residence_type)

data$gender <- ifelse(data$gender == "Female", 1, 
                      ifelse(data$gender == "Other", 2, 0)) #0-Male, 1-Fem, 2-Other 
unique(data$gender)

data$ever_married <- as.numeric(data$ever_married == "Yes") #Yes-1, No-0

data$smoking_status <- ifelse(data$smoking_status == "formerly smoked", 1, 
                      ifelse(data$smoking_status == "smokes", 2, 0))
data$Residence_type <- as.numeric(data$Residence_type == "Urban")

View(data)


#### Descriptive Stats

numeric_columns <- select(data, age, avg_glucose_level, bmi)
summary(numeric_columns)


# Calculate the interquartile range (IQR)
iqr_value_age <- IQR(data$age)
print(iqr_value_age)

iqr_value_bmi <- IQR(data$bmi)
print(iqr_value_bmi)

iqr_value_glucose <- IQR(data$avg_glucose_level)
print(iqr_value_glucose)

count_high_bmi <- sum(data$bmi > 24.9)
print(count_high_bmi)
count_high_bmi*100/3426 #76.7% of people represented in the dataset have higher than normal bmi 

count_high_glucose <- sum(data$avg_glucose_level > 200)
print(count_high_glucose)

count_low_glucose <- sum(data$avg_glucose_level < 70)
print(count_low_glucose)

count_high_glucose*100/3426
count_low_glucose*100/3426


# Create histograms for numerical variables using ggplot2

# Age
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.8) +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 16))


# Average Glucose Level
ggplot(data, aes(x = avg_glucose_level)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black", alpha = 0.8) +
  labs(title = "Histogram of Average Glucose Level", x = "Average Glucose Level", y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 15))

# BMI
ggplot(data, aes(x = bmi)) +
  geom_histogram(binwidth = 2, fill = "salmon", color = "black", alpha = 0.8) +
  labs(title = "Histogram of BMI", x = "BMI", y = "Frequency") +
  theme_minimal() +
  theme(text = element_text(size = 16))

#boxplots for outliers 

library(ggplot2)

ggplot(data, aes(y = age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Age", y = "Age")+
  theme(
    text = element_text(size = 14) # Adjust the text size here
  )

ggplot(data, aes(y = bmi)) +
  geom_boxplot(fill = "salmon", color = "black") +
  labs(title = "Boxplot of BMI", y = "BMI") +
  theme(
    text = element_text(size = 14) # Adjust the text size here
  )

ggplot(data, aes(y = avg_glucose_level)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Average Glucose Level", y = "Average Glucose Level") +
  theme(
    text = element_text(size = 14) # Adjust the text size here
  )


rows_out_bmi <- data[data$bmi > 55, ]

print(rows_out_bmi, n = 50) # none of the observed people had heart_disease, only one had stroke, for many of them 
#average glucose level was normal or lower than normal, so likely data entry errors, will REMOVE

data <- data[!(data$bmi > 55), ]

#let's check average glucose the same way 

rows_out_glucose <- data[data$avg_glucose_level > 250, ]
print(rows_out_glucose)

#let's look at gender distribution

ggplot(data, aes(x = gender)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Gender Distribution", x = "Gender", y = "Frequency")+
  theme(
    text = element_text(size = 14) # Adjust the text size here
  )

count_of_2 <- sum(data$gender == 2)
print(count_of_2) #we see that only one value equals to 2, thats why on graph it is not seen

#for marrige

ggplot(data, aes(x = factor(ever_married))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Ever Married Distribution", x = "Ever Married", y = "Frequency")+
  theme(
    text = element_text(size = 14) # Adjust the text size here
  )

#for smoking

ggplot(data, aes(x = smoking_status)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Smoking Status Distribution", x = "Smoking Status", y = "Frequency")+
  theme(
    text = element_text(size = 14) # Adjust the text size here
  )

#for stroke

ggplot(data, aes(x = factor(stroke))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Stroke Distribution", x = "Stroke", y = "Frequency")+
  theme(
    text = element_text(size = 14) # Adjust the text size here
  )

#for heart_disease
ggplot(data, aes(x = factor(heart_disease))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Heart Disease Distribution", x = "Heart Disease", y = "Frequency")+
  theme(
    text = element_text(size = 14) # Adjust the text size here
  )

#for hypertension
ggplot(data, aes(x = factor(hypertension))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Hypertension Distribution", x = "Hypertension", y = "Frequency")+
  theme(
    text = element_text(size = 14) # Adjust the text size here
  )

#for residence 
ggplot(data, aes(x = factor(Residence_type))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Residence Type Distribution", x = "Residence Type", y = "Frequency") +
  theme(
    text = element_text(size = 14) # Adjust the text size here
  )

#for work
ggplot(data, aes(x = factor(work_type))) +
  geom_bar(fill = "skyblue") +
  labs(title = "Work Type Distribution", x = "Work Type", y = "Frequency") +
  theme(
    text = element_text(size = 14),  # Adjust the text size here
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
library(ggplot2)


###let's check correlation of different variables to stroke 

###numerical

#Point-Biserial Correlation coefficient
correlation_pb_age <- cor.test(data$stroke, data$age)
print(correlation_pb_age)

correlation_pb_bmi <- cor.test(data$stroke, data$bmi)
print(correlation_pb_bmi)

correlation_pb_glucose <- cor.test(data$stroke, data$avg_glucose_level)
print(correlation_pb_glucose)

###categorical

contingency_table_gender <- table(data$stroke, data$gender)
chi_square_result_gender <- chisq.test(contingency_table_gender)
print(chi_square_result_gender)

contingency_table_gender_2 <- table(balanced_data_2$stroke, balanced_data_2$gender)
chi_square_result_gender_2 <- chisq.test(contingency_table_gender)
print(chi_square_result_gender_2)

# Create contingency tables for each categorical variable and 'stroke'
contingency_table_hypertension <- table(data$stroke, data$hypertension)
contingency_table_heart_disease <- table(data$stroke, data$heart_disease)
contingency_table_ever_married <- table(data$stroke, data$ever_married)
contingency_table_work_type <- table(data$stroke, data$work_type)
contingency_table_residence_type <- table(data$stroke, data$Residence_type)
contingency_table_smoking <- table(data$smoking_status, data$smoking_status)

# Perform chi-square tests for each variable
chi_square_result_hypertension <- chisq.test(contingency_table_hypertension)
chi_square_result_heart_disease <- chisq.test(contingency_table_heart_disease)
chi_square_result_ever_married <- chisq.test(contingency_table_ever_married)
chi_square_result_work_type <- chisq.test(contingency_table_work_type)
chi_square_result_residence_type <- chisq.test(contingency_table_residence_type)
chi_square_result_smoking <- chisq.test(contingency_table_smoking)

# Print the results
print("Hypertension:")
print(chi_square_result_hypertension)

print("Heart Disease:")
print(chi_square_result_heart_disease)

print("Ever Married:")
print(chi_square_result_ever_married)

print("Work Type:")
print(chi_square_result_work_type)

print("Residence Type:")
print(chi_square_result_residence_type)

print("Smoking Status:")
print(chi_square_result_smoking)

# Create a bar plot for 'ever_married' and 'stroke'
ggplot(data, aes(x = ever_married, fill = factor(stroke))) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Ever Married", y = "Count", fill = "Stroke") +
  ggtitle("Distribution of Stroke by Marital Status") +
  theme_minimal()

# Create a bar plot for 'work_type' and 'stroke'
ggplot(data, aes(x = work_type, fill = factor(stroke))) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Work Type", y = "Count", fill = "Stroke") +
  ggtitle("Distribution of Stroke by Work Type") +
  theme_minimal()


# Create a bar plot for 'Residence_type' and 'stroke'
ggplot(data, aes(x = Residence_type, fill = factor(stroke))) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Residence Type", y = "Count", fill = "Stroke") +
  ggtitle("Distribution of Stroke by Residence Type") +
  scale_x_discrete(labels = c("Rural", "Urban")) +  # Ensure proper labeling
  theme_minimal()


##############################################################
#convert variables into appropriate formats 
str(data)

# Convert appropriate variables to factors
data$gender <- factor(data$gender)
data$hypertension <- factor(data$hypertension)
data$heart_disease <- factor(data$heart_disease)
data$ever_married <- factor(data$ever_married)
data$smoking_status <- factor(data$smoking_status)
data$Residence_type <- factor(data$Residence_type)
data$stroke <- factor(data$stroke)
data$work_type <- factor(data$work_type)

str(data)

##################################################################################
###split original data on training and testing

library(caret)

# Set the seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
trainIndex <- createDataPartition(data$stroke, p = .75, 
                                  list = FALSE, 
                                  times = 1)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]


# Fit a logistic regression model using the binomial family
logistic_full<- glm(stroke ~. , family = binomial, data = trainData)
summary(logistic_full)
library(rsq)
with(summary(logistic_full), 1 - deviance/null.deviance)
deviance(logistic_full)

# Make predictions on the testing set
predictions <- predict(logistic_full, newdata = testData, type = "response")

# Convert the predicted probabilities to class labels
predicted_classes <- ifelse(predictions > 0.5, "1", "0")

# Evaluate the model's performance (e.g., calculate accuracy)
accuracy <- mean(predicted_classes == testData$stroke)
print(paste("Accuracy:", accuracy))

#f-1 score

CM <- table(testData$stroke, predicted_classes)
print(CM)
#we see that everything was predicted as 0, which can be explained by the imbalance in data

####### we will try balancing the training data and see how the model changes

install.packages("ROSE")
library(ROSE)

balanced_train <- ROSE(heart_disease ~ ., data = trainData, seed = 123)$data
balanced_train_1 <- ROSE(hypertension ~ ., data = balanced_train, seed = 123)$data
balanced_train_2 <- ROSE(stroke ~ ., data = balanced_train_1, seed = 123)$data

#see the difference in observation count 
stroke_counts <- table(trainData$stroke)
print(stroke_counts)
stroke_counts_bal <- table(balanced_train_2$stroke)
print(stroke_counts_bal)

disease_counts <- table(trainData$heart_disease)
print(disease_counts)
disease_counts_bal <- table(balanced_train_2$heart_disease)
print(disease_counts_bal)

hypertension_count <- table(trainData$hypertension)
print(hypertension_count)
hypertension_count_bal <- table(balanced_train_2$hypertension)
print(hypertension_count_bal)

#let's see how heart_disease and stroke look together
ggplot(balanced_train_2, aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Heart Disease", y = "Count", fill = "Stroke") +
  ggtitle("Heart Disease vs. Stroke after balancing") +
  theme_minimal()
ggplot(trainData, aes(x = heart_disease, fill = stroke)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Heart Disease", y = "Count", fill = "Stroke") +
  ggtitle("Heart Disease vs. Stroke before balancing") +
  theme_minimal()

ggplot(trainData, aes(x = stroke, fill = hypertension)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Stroke", y = "Count", fill = "Hypertension") +
  ggtitle("Stroke vs. Hypertension before balancing the data") +
  theme_minimal()

ggplot(balanced_train_2, aes(x = stroke, fill = hypertension)) +
  geom_bar(position = "dodge", color = "black") +
  labs(x = "Stroke", y = "Count", fill = "Hypertension") +
  ggtitle("Stroke vs. Hypertension after balancing the data") +
  theme_minimal()
###############################################################################################
#balanced data stroke prediction


# Fit a logistic regression model using the binomial family
logistic_full_balanced<- glm(stroke ~. , family = binomial, data = balanced_train_2)
summary(logistic_full_balanced)

with(summary(logistic_full_balanced), 1 - deviance/null.deviance)
deviance(logistic_full_balanced)

# Make predictions on the testing set
predictions_balanced <- predict(logistic_full_balanced, newdata = testData, type = "response")

# Convert the predicted probabilities to class labels
predicted_classes_balanced <- ifelse(predictions_balanced > 0.5, "1", "0")

# Evaluate the model's performance (e.g., calculate accuracy)
accuracy_balanced <- mean(predicted_classes_balanced == testData$stroke)
print(paste("Accuracy:", accuracy_balanced))

CM = table(testData$stroke, predicted_classes_balanced)
CM

TN =CM[1,1]
TP =CM[2,2]
FP =CM[1,2]
FN =CM[2,1]
precision =(TP)/(TP+FP)
recall_score =(FP)/(FP+TN)
f1_score=2*((precision*recall_score)/(precision+recall_score))
accuracy_model  =(TP+TN)/(TP+TN+FP+FN)
False_positive_rate =(FP)/(FP+TN)
False_negative_rate =(FN)/(FN+TP)
print(paste("Precision value of the model: ",round(precision,2)))
print(paste("Accuracy of the model: ",round(accuracy_model,2)))
print(paste("Recall value of the model: ",round(recall_score,2)))
print(paste("False Positive rate of the model: ",round(False_positive_rate,2)))
print(paste("False Negative rate of the model: ",round(False_negative_rate,2)))
print(paste("f1 score of the model: ",round(f1_score,2)))

#try reduced model on balanced data 

logistic_reduced_balanced <- glm(stroke~hypertension+heart_disease+ever_married+age+avg_glucose_level+smoking_status,family = binomial, data = balanced_train_2)
summary(logistic_reduced_balanced)
with(summary(logistic_reduced_balanced), 1 - deviance/null.deviance)

predictions_balanced_red <- predict(logistic_reduced_balanced, newdata = testData, type = "response")

# Convert the predicted probabilities to class labels
predicted_classes_balanced_red <- ifelse(predictions_balanced_red > 0.5, "1", "0")

# Evaluate the model's performance (e.g., calculate accuracy)
accuracy_balanced_red <- mean(predicted_classes_balanced_red == testData$stroke)
print(paste("Accuracy:", accuracy_balanced_red))

CM_red = table(testData$stroke, predicted_classes_balanced_red)
CM_red

TN_red =CM_red[1,1]
TP_red =CM_red[2,2]
FP_red =CM_red[1,2]
FN_red =CM_red[2,1]


precision_red =(TP_red)/(TP_red+FP_red)
recall_score_red =(FP_red)/(FP_red+TN_red)
f1_score_red=2*((precision*recall_score_red)/(precision_red+recall_score_red))
accuracy_model_red  =(TP_red+TN_red)/(TP_red+TN_red+FP_red+FN_red)
False_positive_rate_red =(FP_red)/(FP_red+TN_red)
False_negative_rate_red =(FN_red)/(FN_red+TP_red)
print(paste("Precision value of the model: ",round(precision_red,2)))
print(paste("Accuracy of the model: ",round(accuracy_model_red,2)))
print(paste("Recall value of the model: ",round(recall_score_red,2)))
print(paste("False Positive rate of the model: ",round(False_positive_rate_red,2)))
print(paste("False Negative rate of the model: ",round(False_negative_rate_red,2)))
print(paste("f1 score of the model: ",round(f1_score_red,2)))

model_comparison_balanced <- anova(logistic_full_balanced, logistic_reduced_balanced, test = "Chisq")
model_comparison_balanced

##########################################
#try random forest
install.packages("randomForest")
library(randomForest)

formula <- stroke ~ gender + age + hypertension + heart_disease + ever_married + 
  work_type + Residence_type + avg_glucose_level + bmi + smoking_status

rf_model <- randomForest(formula, data = balanced_train_2, ntree = 500)
print(rf_model)

predictions_rf <- predict(rf_model, newdata = testData, type = "response")

predictions_rf_numeric <- as.numeric(as.character(predictions_rf))
predicted_classes_rf <- ifelse(predictions_rf_numeric > 0.5, "1", "0")

CM_rf = table(testData$stroke, predicted_classes_rf)
CM_rf

TN_rf =CM_rf[1,1]
TP_rf =CM_rf[2,2]
FP_rf =CM_rf[1,2]
FN_rf =CM_rf[2,1]
precision_rf =(TP_rf)/(TP_rf+FP_rf)
recall_score_rf =(FP_rf)/(FP_rf+TN_rf)
f1_score_rf=2*((precision_rf*recall_score_rf)/(precision_rf+recall_score_rf))
accuracy_model_rf  =(TP_rf+TN_rf)/(TP_rf+TN_rf+FP_rf+FN_rf)
False_positive_rate_rf =(FP_rf)/(FP_rf+TN_rf)
False_negative_rate_rf =(FN_rf)/(FN_rf+TP_rf)
print(paste("Precision value of the model: ",round(precision_rf,2)))
print(paste("Accuracy of the model: ",round(accuracy_model_rf,2)))
print(paste("Recall value of the model: ",round(recall_score_rf,2)))
print(paste("False Positive rate of the model: ",round(False_positive_rate_rf,2)))
print(paste("False Negative rate of the model: ",round(False_negative_rate_rf,2)))
print(paste("f1 score of the model: ",round(f1_score_rf,2)))

