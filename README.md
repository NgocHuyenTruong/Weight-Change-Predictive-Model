# WEIGHT CHANGE PREDICTIVE MODEL  
## Simulation Modelling Project

---

## 1. Introduction

### 1.1 General Introduction  
This project aims to develop a predictive model to analyze weight change factors using data from 100 participants. The dataset includes information about dietary habits, physical activity, and lifestyle factors.

**Target Variable:**  
- **Weight Change (lbs)** – representing change in weight over a specified period.

**Key Predictors Include:**  
- Daily Calories Consumed  
- Physical Activity Level  
- Stress Level  
- Sleep Quality  
- Basal Metabolic Rate (BMR)

**Data Source:**  
- [Comprehensive Weight Change Prediction Dataset on Kaggle](https://www.kaggle.com)

---

### 1.2 Problem Approach  
The goal is to develop a reliable model despite the challenges posed by a small dataset. We utilize multiple regression techniques and resampling strategies to address overfitting and improve the stability of model estimates.

**Challenges:**  
- Risk of overfitting  
- Non-random residual patterns  
- Unstable regression estimates

**Solution:**  
- Apply **Bootstrap Resampling** to stabilize estimates  
- Use **Residual Diagnostics** to ensure model fit

---

## 2. Understanding the Data

### 2.1 Key Data Attributes  
Explore variables such as age, gender, weight, BMR, caloric intake, sleep quality, stress levels, and physical activity.

---

## 3. Data Inspection

### 3.1 General Inspection  
- Inspect data types, missing values, and summary statistics.

### 3.2 Exploratory Data Analysis (EDA)  
- Analyze distributions, correlations, and pairwise relationships.

---

## 4. Data Preprocessing

### 4.1 Dealing with Outliers  
- Use Z-scores or IQR filtering to handle outliers.

### 4.2 Feature Engineering & Transformation  
- Apply binning and normalization to key variables.

---

## 5. Regression Models

- **Multiple Linear Regression**: Baseline model with all features.
- **Polynomial Regression**: Captures non-linear relationships.
- **Stepwise Regression**: Feature selection using AIC/BIC.
- **Ridge and Lasso Regression**: Regularized models to manage overfitting.

---

## 6. XGBoost & Resampling

- **XGBoost**: Tree-based model for comparison.
- **Bootstrap Estimation of R²**: Confidence intervals for model performance.
- **Jackknife Resampling**: Assess feature importance stability.

---

## 7. Monte Carlo Simulation  
Simulates scenarios to understand weight change outcomes under uncertainty.

---

## 8. Conclusion  
- Regression models uncover relationships between lifestyle factors and weight change.
- Resampling techniques enhance model reliability.
- Insights are valuable for health planning and interventions.

---

## 9. Acknowledgements
- I would like to acknowledge the collaborative efforts of my team members, **Ngoc Anh Nguyen** and **Thanh Dung Nguyen**, in completing this project. I also extend my sincere gratitude to **SUDr. Suchismita Das** for their invaluable guidance and support throughout this research.

