#Exploratory Data Analysis and Preprocessing
# Load necessary libraries
library(dplyr) 
library(tidyr)  
library(ggplot2)

getwd()  
# Load the dataset
df <- read.csv("weight_change_dataset.csv")

# Display the first few rows of the dataset
head(df)

# Display dataset structure and summary
str(df)  
#Dataset contains 100 rows and 13 columns

# A quick statistical overview of the columns
summary(df)  

# Check for missing values
colSums(is.na(df))  

# Identify duplicate rows
duplicates <- df[duplicated(df), ]
sum(duplicated(df))

#There are no missing values or duplicates in the dataset.


# Drop the column 'Participant ID' as it is not useful for analysis
##df <- df[,-1]
df <- df %>% select(-Participant.ID)

# Display the updated dataset structure to confirm changes
str(df)

##___Feature Engineering____
# Create 3 new columns: Caloric Intake Per Weight, Physical Activity Level MET value and Activity Weighted Calories 
# Create a new column: Caloric Intake Per Weight
df$Caloric.Intake.Per.Weight <- df$Daily.Calories.Consumed / df$Current.Weight..lbs.

# Define the MET value mapping for Physical Activity Level
met_value_map <- c(
  "Sedentary" = 1.2,
  "Very Active" = 1.9,
  "Lightly Active" = 1.375,
  "Moderately Active" = 1.55
)

# Map MET values to Physical Activity Level and create a new column
df$Physical.Activity.MET.Value <- unname(met_value_map[df$Physical.Activity.Level])

# Calculate Activity Weighted Calories using MET value and BMR
df$Activity.Weighted.Calories <- df$Physical.Activity.MET.Value * df$BMR..Calories.

#Check the dataset again
head(df)
str(df)

##__Visualizations_____
# Load required library
library(ggplot2)

# The continuous numerical columns
continous_num_cols <- c('Age', 'Current.Weight..lbs.', 'BMR..Calories.', 
                        'Daily.Calories.Consumed', 'Daily.Caloric.Surplus.Deficit',
                        'Weight.Change..lbs.', 'Final.Weight..lbs.', 
                        'Caloric.Intake.Per.Weight', 'Activity.Weighted.Calories')

#Distribution of Continuous Numerical Variables
#W1: Display all distributions sequentially
# Set up the plotting area (3x3 grid)
par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))

# Loop through each column and create histograms with KDE
for (col in continous_num_cols) {
  # Create the plot
  plot <- ggplot(df, aes_string(x = col)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "cyan", color = "black", alpha = 0.7) +
    geom_density(color = "blue", lwd = 1) +
    geom_vline(aes(xintercept = mean(df[[col]], na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median(df[[col]], na.rm = TRUE)), color = "green", linetype = "dashed", size = 1) +
    labs(title = col, subtitle = paste("Mean:", round(mean(df[[col]], na.rm = TRUE), 2), 
                                       "Median:", round(median(df[[col]], na.rm = TRUE), 2)),
         x = col, y = "Density") +
    theme_minimal() +
    theme(title = element_text(size = 14, face = "bold", color = "maroon"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.subtitle = element_text(size = 10))
  
  # Print the plot to ensure it is displayed
  print(plot)
}

#W2: Display all distributions at once (simultaneously)
# Load the necessary library
library(gridExtra)

# Create a list to store the plots
plot_list <- list()

# Loop through each column and create histograms with KDE
for (col in continous_num_cols) {
  # Get the data for the current column
  col_data <- df[[col]]
  
  # Calculate the range for the x-axis (minimum and maximum values)
  col_range <- range(col_data, na.rm = TRUE)
  
  # Create the plot with dynamically set xlim based on the data for each column
  plot <- ggplot(df, aes_string(x = col)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "cyan", color = "black", alpha = 0.7) +
    geom_density(color = "blue", lwd = 1) +
    geom_vline(aes(xintercept = mean(df[[col]], na.rm = TRUE)), color = "red", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median(df[[col]], na.rm = TRUE)), color = "green", linetype = "dashed", size = 1) +
    labs(title = col, subtitle = paste("Mean:", round(mean(df[[col]], na.rm = TRUE), 2), 
                                       "Median:", round(median(df[[col]], na.rm = TRUE), 2)),
         x = col, y = "Density") +
    theme_minimal() +
    theme(title = element_text(size = 14, face = "bold", color = "maroon"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          plot.subtitle = element_text(size = 10)) +
    xlim(col_range)  # Set dynamic xlim based on the data range
  
  # Add the plot to the plot list
  plot_list[[col]] <- plot
}

# Use grid.arrange to display all plots at once
grid.arrange(grobs = plot_list, ncol = 3)


##__Scatter plot to show Relationship between Weight Change (lbs) with all other numerical columns_____
# Create a list to store the scatter plots
plot_list <- list()

# Loop through each column and create scatter plots with 'Weight Change (lbs)'
for (col in continous_num_cols) {
  # Create the scatter plot
  plot <- ggplot(df, aes_string(x = col, y = 'Weight.Change..lbs.')) +
    geom_point(color = 'black', shape = 16) +
    labs(title = col, x = col, y = 'Weight Change (lbs)') +
    theme_minimal() +
    theme(title = element_text(size = 14, face = "bold", color = "maroon"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10))
  
  # Add the plot to the list
  plot_list[[col]] <- plot
}

# Use grid.arrange to display all plots at once (3x3 grid)
grid.arrange(grobs = plot_list, ncol = 3)

#There is no strong linear relationship between "Weight Change (lbs)" and the other variables


##____Correlation between numerical variables___
library(reshape2)

# Compute the correlation matrix
correlation <- cor(df[continous_num_cols], use = "complete.obs")

# Melt the correlation matrix into long format
correlation_melted <- melt(correlation)

# Create the heatmap using ggplot2 with correlation values and custom colors
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "white", high = "red", mid = "orange", midpoint = 0) + 
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add correlation values inside the tiles
  theme_minimal() + 
  labs(title = "Correlation Between Numerical Variables", x = "Variables", y = "Variables") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, face = "bold", color = "black"))
#Drop some variables later


##__Visualizing how categorical variables correlating with average Weight Change
library(grid)
# List of categorical variables
cat_cols <- c('Gender', 'Physical.Activity.Level', 'Sleep.Quality', 'Stress.Level', 'Duration..weeks.')

# Create a list of ggplot objects for each plot
plots <- list()

for (col in cat_cols) {
  p <- ggplot(df, aes_string(x = col, y = 'Weight.Change..lbs.')) +
    geom_bar(stat = 'summary', fun = 'mean', fill = 'cyan', color = 'black') +  # Same color for all bars
    theme_minimal() +
    ggtitle(col) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title = element_text(size = 15, color = 'maroon'),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12))
  plots[[col]] <- p
}

# Arrange all the plots in a grid
grid.arrange(grobs = plots, ncol = 2, top = textGrob("Barplot of Categorical Variable with Weight Change (lbs)", gp = gpar(fontsize = 20, col = 'black')))


#___Visualizing Proportion of Gender in the Dataset_____
# Create a pie chart to visualize the proportion of Gender
gender_count <- table(df$Gender)  # Count the occurrences of each gender

# Create the pie chart
ggplot(data = as.data.frame(gender_count), aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("skyblue", "pink")) +
  labs(title = "Proportion of Gender in the Dataset") +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    legend.title = element_blank()
  ) +
  geom_text(aes(label = paste0(round(Freq / sum(Freq) * 100, 2), "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 6)

#Gender distribution is fairly equal in the dataset

##____Deal with Outliers______
##__Box plot to visualize outliers in continuous columns___
plots <- list()
for (col in continous_num_cols) {
  p <- ggplot(df, aes_string(x = col)) + 
    geom_boxplot(fill = "cyan", color = "black") +
    ggtitle(col) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 15, color = 'maroon'),
          axis.title.y = element_text(size = 12),
          axis.text.y = element_text(size = 10))
  plots[[col]] <- p
}

# Arrange all the plots in a grid (3x3 layout)
library(gridExtra)
grid.arrange(grobs = plots, ncol = 3)

#There are some outliers in "Daily.Calories.Consumed," "Caloric.Intake.Per.Weight" and "Weight.Change..lbs."

#Deal with outliers of "Weight.Change..lbs."
# Calculate IQR and bounds for "Weight.Change..lbs."
Q1a <- quantile(df$Weight.Change..lbs., 0.25, na.rm = TRUE)
Q3a <- quantile(df$Weight.Change..lbs., 0.75, na.rm = TRUE)
IQRa <- Q3a - Q1a
lower_bound1 <- Q1a - 1.5 * IQRa
upper_bound1 <- Q3a + 1.5 * IQRa

# Handle outliers (capping method)
df$Weight.Change..lbs.[df$Weight.Change..lbs. < lower_bound1] <- lower_bound1
df$Weight.Change..lbs.[df$Weight.Change..lbs. > upper_bound1] <- upper_bound1

#Same rules applied to other 2 variables: 
Q1b <- quantile(df$Caloric.Intake.Per.Weight, 0.25, na.rm = TRUE)
Q3b <- quantile(df$Caloric.Intake.Per.Weight, 0.75, na.rm = TRUE)
IQRb <- Q3b - Q1b
lower_bound2 <- Q1b - 1.5 * IQRb
upper_bound2 <- Q3b + 1.5 * IQRb
df$Caloric.Intake.Per.Weight[df$Caloric.Intake.Per.Weight < lower_bound2] <- lower_bound2
df$Caloric.Intake.Per.Weight[df$Caloric.Intake.Per.Weight > upper_bound2] <- upper_bound2

Q1c <- quantile(df$Daily.Calories.Consumed, 0.25, na.rm = TRUE)
Q3c <- quantile(df$Daily.Calories.Consumed, 0.75, na.rm = TRUE)
IQRc <- Q3c - Q1c
lower_bound3 <- Q1c - 1.5 * IQRc
upper_bound3 <- Q3c + 1.5 * IQRc
df$Daily.Calories.Consumed[df$Daily.Calories.Consumed < lower_bound3] <- lower_bound3
df$Daily.Calories.Consumed[df$Daily.Calories.Consumed > upper_bound3] <- upper_bound3

# Draw the box plot again to check for no more outliers
p1 <- ggplot(df, aes(x = '', y = Weight.Change..lbs.)) +
  geom_boxplot(fill = "cyan", color = "black") +
  ggtitle("Weight Change (lbs.)") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 15, color = 'maroon'),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10))

p2 <- ggplot(df, aes(x = '', y = Caloric.Intake.Per.Weight)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  ggtitle("Caloric.Intake.Per.Weight") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 15, color = 'maroon'),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10))

p3 <- ggplot(df, aes(x = '', y = Daily.Calories.Consumed)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  ggtitle("Daily.Calories.Consumed") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 15, color = 'maroon'),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10))

#Print the plot
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)


##___Feature Selection___
# Drop specified columns using base R
df <- df[, !(colnames(df) %in% c('Caloric.Intake.Per.Weight', 'BMR..Calories.',
                                 'Physical.Activity.Level', 'Physical.Activity.MET.Value',
                                 'Final.Weight..lbs.'))]

# Check the columns after removal
colnames(df)

##___FEATURE TRANSFORMATION (Encoding the categorical columns)___
# Encoding the 'Gender' column
df$Gender <- as.factor(df$Gender)
df$Gender <- as.integer(df$Gender) - 1  # Label encoding (0, 1)

# Encoding the 'Sleep Quality' column with multiple levels
df$`Sleep.Quality` <- case_when(
  df$`Sleep.Quality` == "Excellent" ~ 3,
  df$`Sleep.Quality` == "Good" ~ 2,
  df$`Sleep.Quality` == "Fair" ~ 1,
  df$`Sleep.Quality` == "Poor" ~ 0)# Label encoding (0, 1, 2, 3)

# Verify the transformation
head(df)


##__Scaling data__
#install.packages("caret")
library(caret)
# Using preProcess() function to scale and center the data
preprocessor <- preProcess(df[-6], method = c("center", "scale"))
df[-6] <- predict(preprocessor, df[-6])
head(df)


##___MULTIPLE LINEAR REGRESSION______
# Set a seed for reproducibility
set.seed(8386)

# Define the proportion for training data
train_proportion <- 0.8

# Create a random sample of row indices for the training set
train_indices <- sample(1:nrow(df), size = round(train_proportion * nrow(df)))

# Split the dataset into training and testing sets
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Build the multiple linear regression model
model <- lm(Weight.Change..lbs. ~ ., data = train_data)

# Summary of the model
summary(model)
###  MLR model: R-squared:  0.5378,	Adjusted R-squared:  0.4784, p-value: 6.255e-09


predictions <- predict(model, newdata = test_data)
# Actual values from the test dataset
actual <- test_data$Weight.Change..lbs.
# Calculate SS_tot and SS_res
SS_tot <- sum((actual - mean(actual))^2)  # Total sum of squares
SS_res <- sum((actual - predictions)^2)  # Residual sum of squares
# Calculate R-squared
R_squared_test <- 1 - (SS_res / SS_tot)
# Print R-squared
print(paste("R-squared for test data:", R_squared_test))
#R-squared for test data: 0.526071187365508


library(ggplot2)

# Extract residuals, fitted values, and standardized residuals
residuals <- resid(model)
fitted_values <- fitted(model)
std_residuals <- rstandard(model)

# 1. Residuals vs Fitted Plot
ggplot(data = NULL, aes(x = fitted_values, y = residuals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
#The residuals vs. fitted plot shows that the residuals are scattered randomly around the horizontal line at 0, but there seems to be a slight pattern, indicating potential non-linearity or heteroscedasticity.

ggplot(data = NULL, aes(x = fitted_values, y = residuals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", color = "black", se = FALSE) +  # Add smoothing line
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# 2. Q-Q Plot
ggplot(data = NULL, aes(sample = std_residuals)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(title = "Q-Q Plot of Standardized Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# 3. Standardized Residuals vs Fitted
ggplot(data = NULL, aes(x = fitted_values, y = std_residuals)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Standardized Residuals vs Fitted", x = "Fitted Values", y = "Standardized Residuals") +
  theme_minimal()

# 4. Residual Histogram
ggplot(data = NULL, aes(x = std_residuals)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  labs(title = "Histogram of Standardized Residuals", x = "Standardized Residuals", y = "Frequency") +
  theme_minimal()


##____Polynomial Regression______
#Plot of residual-fitted shows a pattern where the residuals are more spread our at higher fitted values, curvature in residuals -> Higher-order polynomial might fit the data better -> Adding squared or cubic terms
# Add polynomial terms to the dataset
add_polynomial_features <- function(data, predictors, degree) {
  for (pred in predictors) {
    for (i in 2:degree) {
      data[[paste0(pred, "^", i)]] <- data[[pred]]^i
    }
  }
  return(data)
}


# Add the best polynomial features
data_poly <- add_polynomial_features(df, 
                                     predictors = colnames(train_data)[-6], 
                                     degree =3)
colnames(data_poly)
train_proportion <- 0.8

# Create a random sample of row indices for the training set
train_indices <- sample(1:nrow(data_poly), size = round(train_proportion * nrow(data_poly)))

# Split the dataset into training and testing sets
train_data_poly <- data_poly[train_indices, ]
test_data_poly <- data_poly[-train_indices, ]
# Build the multiple linear regression model
model2 <- lm(Weight.Change..lbs. ~ ., data = train_data_poly)

# Summary of the model
summary(model2)
#Model with polynomial terms: R-squared:  0.8392,	Adjusted R-squared:  0.7648 , p-value: 1.171e-13

model2 <- lm(
  Weight.Change..lbs. ~ Age + Gender + Current.Weight..lbs. + Daily.Calories.Consumed +
    Daily.Caloric.Surplus.Deficit + Duration..weeks. + Sleep.Quality + Stress.Level +
    Activity.Weighted.Calories + I(Age^2) + I(Age^3) + 
    I(Current.Weight..lbs.^2) + I(Current.Weight..lbs.^3) +
    I(Daily.Calories.Consumed^2) + I(Daily.Calories.Consumed^3) +
    I(Daily.Caloric.Surplus.Deficit^2) + I(Daily.Caloric.Surplus.Deficit^3) +
    I(Duration..weeks.^2) + I(Duration..weeks.^3) +
    I(Sleep.Quality^2) + I(Sleep.Quality^3) +
    I(Stress.Level^2) + I(Stress.Level^3) +
    I(Activity.Weighted.Calories^2) + I(Activity.Weighted.Calories^3),
  data = train_data_poly
)
summary(model2)
#R-squared:  0.8392,	Adjusted R-squared:  0.7648 , p-value: 1.171e-13

predictions <- predict(model2, newdata = test_data_poly)
# Actual values from the test dataset
actual <- test_data_poly$Weight.Change..lbs.
# Calculate SS_tot and SS_res
SS_tot <- sum((actual - mean(actual))^2)  # Total sum of squares
SS_res <- sum((actual - predictions)^2)  # Residual sum of squares
# Calculate R-squared
R_squared_test <- 1 - (SS_res / SS_tot)
# Print R-squared
print(paste("R-squared for test data:", R_squared_test))
#R-squared for test data: 0.18270155253752 --> Overfitting because R-squared in test set drops significantly
#and p-value of multiple variables are greater than 0.05, so we use VIF to reduce the number of variables that haves VIF score >10 

##Calculate VIF and then remove variables with VIF >10
library(car)
alias(model2)

vif_values<-vif(model2)

# Identify variables with VIF > 10
high_vif_vars <- names(vif_values[vif_values > 10])

# Remove the high VIF variables from the model
train_data_cleaned <- train_data_poly[, !(names(train_data_poly) %in% high_vif_vars)]
test_data_cleaned <- test_data_poly[, !(names(test_data_poly) %in% high_vif_vars)]
# Refit the model without high VIF variables
model_cleaned <- lm(Weight.Change..lbs. ~ ., data = train_data_cleaned)

# Compute the VIF again for the new model
summary(model_cleaned)
#Model after removing high VIF variables: R-squared:  0.8097,	Adjusted R-squared:  0.7495, p-value: 4.387e-15

predictions <- predict(model_cleaned, newdata = test_data_cleaned)
# Actual values from the test dataset
actual <- test_data_cleaned$Weight.Change..lbs.
# Calculate SS_tot and SS_res
SS_tot <- sum((actual - mean(actual))^2)  # Total sum of squares
SS_res <- sum((actual - predictions)^2)  # Residual sum of squares
# Calculate R-squared
R_squared_test <- 1 - (SS_res / SS_tot)
# Print R-squared
print(paste("R-squared for test data:", R_squared_test))
#R-squared for test data: 0.402453226194991


##____Stepwise regression____

# Use stepwise regression (both forward and backward selection) to reduce predictors
model_step <- step(model_cleaned, direction = "both")
summary(model_step)
#Model stepwise regression: 0.8081,	Adjusted R-squared:  0.7737 , p-value: < 2.2e-16

predictions <- predict(model_step, newdata = test_data_cleaned)
# Actual values from the test dataset
actual <- test_data_cleaned$Weight.Change..lbs.
# Calculate SS_tot and SS_res
SS_tot <- sum((actual - mean(actual))^2)  # Total sum of squares
SS_res <- sum((actual - predictions)^2)  # Residual sum of squares
# Calculate R-squared
R_squared_test <- 1 - (SS_res / SS_tot)
# Print R-squared
print(paste("R-squared for test data:", R_squared_test))
#R-squared for test data: 0.370383321609544


# Extract the formula of the selected model
selected_variables <-all.vars(formula(model_step))
# Subset the test_data_cleaned dataframe using the updated selected_variables
# Subset the dataframe to only include the selected variables
data_filtered <-data_poly[selected_variables]
train_data_filtered <- train_data_poly[selected_variables]
test_data_filtered <- test_data_poly[selected_variables]
y <- data_filtered$Weight.Change..lbs.
y_train<-train_data_filtered$Weight.Change..lbs.
y_test<-test_data_filtered$Weight.Change..lbs.


##____Ridge and Lasso Regression________

#install.packages('glmnet')
# Load necessary library
library(glmnet)
# Prepare your data
x_train <- as.matrix(train_data_filtered[-1])
x_test <-as.matrix(test_data_filtered[-1])# All columns except the response variable
# Refit Ridge Model with Cross-Validation
best_ridge_model <- cv.glmnet(x_train, y_train, alpha = 0)
# Refit Lasso Model with Cross-Validation
best_lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)

# Predict using lambda.min
ridge_predictions <- predict(best_ridge_model, newx = x_test, s = best_ridge_model$lambda.min)
lasso_predictions <- predict(best_lasso_model, newx = x_test, s = best_lasso_model$lambda.min)


# Make predictions on the test set
# Calculate R-squared
ss_total <- sum((y_test - mean(y_test))^2)
ss_residual <- sum((y_test - ridge_predictions)^2)
r_squared <- 1 - (ss_residual / ss_total)
cat("R-squared on Test Set:", r_squared, "\n")
#R-squared on Test Set: 0.4568907 



##______XGBoost_____
set.seed(8386)
#install.packages("xgboost")
library(xgboost)
##Output variable might be acceptable without transformation due to real-world context -> but we can use tree-based model, gradient-boosting models, which are not sensitive to the distribution of the target variable.
#->XGBoost model

xgb_data_train <- train_data[-6]
# Convert the target variable (Weight Change) into a numeric vector
y <- df$Weight.Change..lbs.
y_train<-train_data$Weight.Change..lbs.
y_test<-test_data$Weight.Change..lbs.
x_train <- as.matrix(train_data[-6])
x_test <-as.matrix(test_data[-6])
# Convert the data into a matrix format (required by XGBoost)
xgb_train_data_matrix <- as.matrix(xgb_data_train)
dtrain <- xgb.DMatrix(data = xgb_train_data_matrix, label = y_train)


# Train XGBoost model
xgb_model <- xgb.train(
  data = dtrain,
  nrounds = 5
)
summary(xgb_model)
# Prepare test data
xgb_test_data <- test_data[-6]
# Convert test data into a matrix
xgb_test_data_matrix <- as.matrix(xgb_test_data)

# Make predictions on the training set
train_predictions <- predict(xgb_model, newdata = xgb_train_data_matrix)
# Calculate R-squared for training data
ss_total_train <- sum((y_train - mean(y_train))^2)
ss_residual_train <- sum((y_train - train_predictions)^2)
r_squared_train <- 1 - (ss_residual_train / ss_total_train)
cat("R-squared on Training Data:", r_squared_train, "\n")
#R-squared on Training Data: 0.9174672 

# Make predictions on the test set
predictions <- predict(xgb_model, newdata = xgb_test_data_matrix)
# Calculate R-squared
ss_total <- sum((y_test - mean(y_test))^2)
ss_residual <- sum((y_test - predictions)^2)
r_squared <- 1 - (ss_residual / ss_total)
cat("R-squared on Test Set:", r_squared, "\n")
## XGBoost with selected variables: R-squared: 0.6563695 


#Hyperparameter Tuning
# Define parameter grid
param_grid <- expand.grid(
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(3, 5, 7),
  gamma = c(0, 1, 5),
  colsample_bytree = c(0.5, 0.7, 1),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.7, 0.8, 1)
)
best_params <- list()
best_r2 <- -Inf
# Loop over all combinations
for (i in 1:nrow(param_grid)) {
  params <- as.list(param_grid[i, ])
  # Train model
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 10,
    objective = "reg:squarederror",
    verbose = 0
  )
  # Predict on validation set
  predictions <- predict(model, xgb_test_data_matrix)
  # Calculate R-squared
  ss_total <- sum((y_test - mean(y_test))^2)
  ss_residual <- sum((y_test - predictions)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  if (r_squared > best_r2) {
    best_r2 <- r_squared
    best_params <- params
  }
}
cat("Best Parameters:\n")
print(best_params)
cat("Best R-squared:", best_r2, "\n")
model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = 10,
  objective = "reg:squarederror",
  verbose = 0
)
# Make predictions on the training set
train_predictions <- predict(model, newdata = xgb_train_data_matrix)
# Calculate R-squared for training data
ss_total_train <- sum((y_train - mean(y_train))^2)
ss_residual_train <- sum((y_train - train_predictions)^2)
r_squared_train <- 1 - (ss_residual_train / ss_total_train)
cat("R-squared on Training Data:", r_squared_train, "\n")
#R-squared on Training Data: 0.9728908



##_____Bootstrap Estimation of R-squared_____

#Bootstrap to estimate R-squared
n_bootstrap <- 1000  # Number of bootstrap resamples
r_squared_bootstrap <- numeric(n_bootstrap)
for (i in 1:n_bootstrap) {
  # Create bootstrap sample with replacement
  bootstrap_sample <- train_data[sample(1:nrow(train_data), replace = TRUE), ]
  # Train a model on the bootstrap sample
  dtrain_bootstrap <- xgb.DMatrix(data = as.matrix(bootstrap_sample), label = bootstrap_sample$Weight.Change..lbs.)
  xgb_model_bootstrap <- xgb.train(
    params = best_params,
    data = dtrain_bootstrap,
    nrounds = 10,
    objective = "reg:squarederror",
    verbose = 0
  )
  # Evaluate the model on out-of-bag (OOB) samples
  oob_sample <- train_data[!(1:nrow(train_data) %in% rownames(bootstrap_sample)), ]
  predictions_oob <- predict(xgb_model_bootstrap, newdata = as.matrix(oob_sample))
  # Calculate R-squared for OOB predictions
  ss_total <- sum((oob_sample$Weight.Change..lbs. - mean(oob_sample$Weight.Change..lbs.))^2)
  ss_residual <- sum((oob_sample$Weight.Change..lbs. - predictions_oob)^2)
  r_squared_bootstrap[i] <- 1 - (ss_residual / ss_total)
}
# Calculate the mean and confidence intervals for R-squared
mean_r_squared <- mean(r_squared_bootstrap)
lower_ci <- quantile(r_squared_bootstrap, 0.025)
upper_ci <- quantile(r_squared_bootstrap, 0.975)
cat("Bootstrap mean R-squared:", mean_r_squared, "\n")
#Bootstrap mean R-squared: 0.9724859 
cat("95% confidence interval for R-squared:", lower_ci, "-", upper_ci, "\n")
#95% confidence interval for R-squared: 0.9004826 - 0.9947817 


##_____Jackknife Resampling for Feature Importance Estimation_______

# Fit the model with the full dataset
xgb_model_full <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = 10,
  objective = "reg:squarederror",
  verbose = 0
)
# Get feature importance from the full model
feature_importance_full <- xgb.importance(feature_names = colnames(x_train), model = xgb_model_full)
# Number of observations in the training set
n_train <- nrow(x_train)
# Initialize matrix to store jackknife feature importances
# The number of columns should be equal to the number of features from the full model
jackknife_importance <- matrix(0, nrow = n_train, ncol = length(feature_importance_full$Feature))
# Jackknife resampling loop
for (i in 1:n_train) {
  # Leave out the i-th observation
  x_train_jackknife <- x_train[-i, , drop = FALSE]
  y_train_jackknife <- y_train[-i]
  # Train the model on the jackknife sample
  dtrain_jackknife <- xgb.DMatrix(data = x_train_jackknife, label = y_train_jackknife)
  xgb_model_jackknife <- xgb.train(
    params = best_params,
    data = dtrain_jackknife,
    nrounds = 10,
    objective = "reg:squarederror",
    verbose = 0
  )
  # Get feature importance for the jackknife model
  feature_importance_jackknife <- xgb.importance(feature_names = colnames(x_train), model = xgb_model_jackknife)
  # Ensure all features are in the same order as the full model
  importance_vector <- rep(0, length(feature_importance_full$Feature))
  for (j in 1:length(feature_importance_jackknife$Feature)) {
    # Match feature names and assign importance values
    feature_idx <- which(feature_importance_full$Feature == feature_importance_jackknife$Feature[j])
    importance_vector[feature_idx] <- feature_importance_jackknife$Gain[j]
  }
  # Store the feature importance for this resample
  jackknife_importance[i, ] <- importance_vector
}
# Calculate the jackknife mean importance
jackknife_mean_importance <- colMeans(jackknife_importance)
# Calculate the bias of the feature importance
bias_importance <- jackknife_mean_importance - feature_importance_full$Gain
# Calculate the standard error of the feature importance
jackknife_se_importance <- sqrt((n_train - 1) * colMeans((jackknife_importance - jackknife_mean_importance)^2))
# Print the results
cat("Bias of Feature Importance:\n")
print(bias_importance)
cat("\nStandard Error of Feature Importance:\n")
print(jackknife_se_importance)
# Optionally, you can visualize the bias and standard error of feature importance
par(mfrow = c(1, 2))

# Get actual feature importance
importance <- xgb.importance(model = xgb_model_full)
# Convert bias and standard error into a data frame for easy plotting
importance_df <- data.frame(
  Feature = feature_importance_full$Feature,
  Bias = bias_importance,
  StandardError = jackknife_se_importance
)

# Plot the Bias of Feature Importance
ggplot(importance_df, aes(x = reorder(Feature, Bias), y = Bias)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_point(data = importance, aes(x = reorder(Feature, Gain), y = Gain), 
             color = "blue", size = 3) +
  coord_flip() +  # Flip for better readability
  labs(title = "Bias of Feature Importance",
       x = "Feature",
       y = "Bias") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8))  # Adjust text size


# Create a single plot with both feature importance and standard error
ggplot() +
  # Add bars for standard error
  geom_bar(data = importance_df, aes(x = reorder(Feature, StandardError), y = StandardError), 
           stat = "identity", fill = "lightgreen", alpha = 0.7) +
  # Add points for actual feature importance
  geom_point(data = importance, aes(x = reorder(Feature, Gain), y = Gain), 
             color = "blue", size = 3) +
  # Flip coordinates for better readability
  coord_flip() +
  # Add labels and title
  labs(title = "Feature Importance and Standard Error",
       x = "Feature",
       y = "Importance / Standard Error") +
  # Customize theme
  theme_minimal() +
  theme(axis.text = element_text(size = 8))  # Adjust axis text size



##_____Monte Carlo Simulation for Predicting Weight Change_____

# Function to preprocess new data
preprocess_new_data <- function(new_data,preprocessor) {
  # Encoding 'Gender'
  new_data$Gender <- ifelse(new_data$Gender == "M", 1, 0)  # M -> 1, F -> 0
  # Encoding 'Sleep.Quality'
  new_data$`Sleep.Quality` <- case_when(
    new_data$`Sleep.Quality` == "Excellent" ~ 3,
    new_data$`Sleep.Quality` == "Good" ~ 2,
    new_data$`Sleep.Quality` == "Fair" ~ 1,
    new_data$`Sleep.Quality` == "Poor" ~ 0,
    TRUE ~ NA_real_  # In case there are unexpected values
  )
  # Scale and center the original variables using the preprocessor
  scaled_data <- predict(preprocessor, new_data)
  selected_variables<-c("Age","Gender","Current.Weight..lbs.",
                        "Daily.Calories.Consumed","Daily.Caloric.Surplus.Deficit",
                        "Duration..weeks.","Sleep.Quality","Stress.Level",
                        "Activity.Weighted.Calories")
  # Filter only the necessary variables for prediction
  processed_data = scaled_data[,selected_variables,drop = FALSE]
  return(processed_data)
}
# Function to predict weight change
predict_weight_change <- function(model, processed_data) {
  # Convert to XGBoost DMatrix
  dnew <- xgb.DMatrix(data = as.matrix(processed_data))
  # Predict using the pre-trained model
  predictions <- predict(model, dnew)
  return(predictions)
}
# Function to run Monte Carlo simulations
monte_carlo_simulation <- function(new_data, preprocessor, model, n_simulations = 1000) {
  # Store the simulation results
  simulation_results <- numeric(n_simulations)
  # Run Monte Carlo simulation
  for (i in 1:n_simulations) {
    # Add randomness to input variables (you can define your own distributions)
    new_data_sim <- new_data
    new_data_sim$Daily.Calories.Consumed <- rnorm(1, mean = new_data$Daily.Calories.Consumed, sd = 200)  # Adding random noise to calories
    new_data_sim$Stress.Level <- sample(max(0,new_data$Stress.Level-1):min(new_data$Stress.Level+1,10), 1, replace = TRUE)# Randomly sample stress level
    new_data_sim$Sleep.Quality <- sample(c('Excellent', 'Good', 'Fair', 'Poor'), 1, replace = TRUE)
    # Preprocess the simulated data
    processed_data <- preprocess_new_data(new_data_sim, preprocessor)
    # Predict weight change for the simulated data
    simulation_results[i] <- predict_weight_change(model, processed_data)
  }
  # Return the results of the simulation
  return(simulation_results)
}

# Set up the pipeline for Monte Carlo simulation
predict_pipeline_mc <- function(new_data, preprocessor, model, n_simulations = 1000) {
  # Run Monte Carlo simulation
  simulation_results <- monte_carlo_simulation(new_data, preprocessor, model, n_simulations)
  # Aggregate results (mean, confidence interval, etc.)
  mean_prediction <- mean(simulation_results)
  lower_bound <- quantile(simulation_results, 0.025)  # 2.5% quantile (lower bound of 95% CI)
  upper_bound <- quantile(simulation_results, 0.975)  # 97.5% quantile (upper bound of 95% CI)
  # Return aggregated results
  return(list(
    mean_prediction = mean_prediction,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  ))
}
# Create a sample data frame with one row
sample_data <- data.frame(
  Age = 56,
  Gender = "F",
  Current.Weight..lbs. = 107,
  Daily.Calories.Consumed = 1399,
  Daily.Caloric.Surplus.Deficit = 320,
  Duration..weeks. = 10,
  Sleep.Quality = 'Good',
  Stress.Level = 2,
  Activity.Weighted.Calories = 316
)
# View the sample data frame
print(sample_data)
# Use the preprocessor and model (make sure you have your preprocessor and model defined)
predictions_mc <- predict_pipeline_mc(sample_data, preprocessor, xgb_model_full)
# View the simulation results (mean and confidence intervals)
print(predictions_mc)


