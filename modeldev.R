# Load necessary libraries
library(caret)
library(randomForest)
library(readr)
library(pROC)

# Load the dataset
df <- read.csv("https://raw.githubusercontent.com/DataGuy-Kariuki/Credit-Risk-Modelling/refs/heads/main/credit_risk_large_dataset_v2.csv")

# Data preprocessing
df$Income[is.na(df$Income)] <- median(df$Income, na.rm = TRUE)
df$Debt_Income_Ratio <- df$Loan_Amount / df$Income

# One-hot encoding of categorical variables
df_dummy <- dummyVars("~ .", data = df)
df_transformed <- predict(df_dummy, newdata = df)
df_one_hot <- as.data.frame(df_transformed)

# Split data into training (70%) and testing (30%)
set.seed(123)
trainIndex <- createDataPartition(df$Loan_Status, p = 0.7, list = FALSE)
train <- df_one_hot[trainIndex, ]  # Use one-hot encoded data
test <- df_one_hot[-trainIndex, ]   # Use one-hot encoded data

# Train the Random Forest model
rf_model <- randomForest(as.factor(Loan_Status) ~ ., data = train, ntree = 100)


# Making predictions on the test data
rf_pred <- predict(rf_model, test)
rf_pred

# Evaluate the model
conf_matrix <- confusionMatrix(as.factor(rf_pred), as.factor(test$Loan_Status))
print(conf_matrix)

# ROC curve
roc_obj <- roc(test$Loan_Status, as.numeric(rf_pred))
plot(roc_obj, col = "blue", main = "ROC CURVE")


# Load the dataset
past_defaults <- read.csv("https://raw.githubusercontent.com/DataGuy-Kariuki/Credit-Risk-Modelling/refs/heads/main/past_defaults.csv")

# Ensure the Past_Defaults variable exists and is treated as a factor
if (!"Past_Defaults" %in% names(past_defaults)) {
  stop("Past_Defaults variable not found in the dataset.")
}

# Convert Past_Defaults to a factor for classification
past_defaults$Default <- as.factor(past_defaults$Past_Defaults)

# Train the Random Forest model for classification
rf_model <- randomForest(Default ~ Age + Income + Loan_Amount + Credit_Score + Gender, data = past_defaults)

# Save the model to an RDS file for later use
saveRDS(rf_model, file = "rf_model.rds")
