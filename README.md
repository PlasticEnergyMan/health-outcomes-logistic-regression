# Logistic Regression Modeling of Health Outcomes

## Overview
This project applies logistic regression to analyze clinical predictors of patient outcomes using health record data. The goal is to build an interpretable statistical model and evaluate its performance using classification metrics and validation techniques.

## Methods
- Logistic regression
- Variable selection and model refinement
- ROC and AUC analysis
- Sensitivity and specificity evaluation
- Cross-validation

## Evaluation
Model performance was assessed using ROC/AUC, confusion matrices, and validation procedures to balance interpretability and predictive accuracy.

## Key Findings
- A reduced logistic regression model achieved strong predictive performance while remaining interpretable  
- Age, serum creatinine, ejection fraction, and time were statistically significant predictors of outcomes  
- The final model demonstrated high discriminative ability (AUC ≈ 0.89)  
- ROC analysis illustrated tradeoffs between sensitivity and specificity depending on classification thresholds  

## Data
The dataset consists of clinical health records used for educational and analytical purposes. The raw data file is not included in this repository.

## Repo Structure
- `analysis/`: R script containing data preparation, modeling, and evaluation
- `report/`: final project report (PDF)

## How to Run
1. Open the script in `analysis/` using RStudio  
2. Install required packages listed at the top of the file  
3. Run the script sequentially to reproduce results

