# Predicting REM Sleep Duration Based on Lifestyle Habits

## Author
Glynn Smith

## Overview
This project explores the relationship between exercise frequency and REM sleep duration while developing a predictive model using various lifestyle habits. The dataset includes 452 participants' self-reported and actigraphy-collected sleep and lifestyle data. Multiple linear regression modeling techniques are applied to identify significant predictors and evaluate their impact on REM sleep duration.

## Dataset
- **Source**: [Kaggle](https://www.kaggle.com/datasets/equilibriumm/sleep-efficiency/data) (collected by AI Engineering students at the High National School for Computer Science, Morocco)
- **Size**: 452 observations (reduced to 388 after removing missing values)
- **Features**:
  - **Demographics**: Age, Gender
  - **Sleep Metrics**: Bedtime, Wake-up time, Sleep duration, Sleep efficiency, REM sleep percentage, Deep sleep percentage, Light sleep percentage, Number of awakenings
  - **Lifestyle Factors**: Caffeine consumption, Alcohol consumption, Smoking status, Exercise frequency

## Objectives
- Identify whether exercise frequency significantly affects REM sleep duration.
- Develop a predictive model to estimate REM sleep duration based on lifestyle habits.

## Methodology
### Data Cleaning & Transformation
- Removed missing values (64 observations dropped).
- Created the dependent variable `REM sleep time` using available sleep metrics.
- Applied log transformation to normalize the dependent variable.

#### Figure 1: Log Transformd Preliminary Model Residual Diagnostics
![image](https://github.com/user-attachments/assets/aab52a04-b646-48c5-b55c-26b9109a3128)


### Exploratory Data Analysis
- Visualized distributions using box plots and bar charts.
#### Figure 2: Box Plots of Numerical Variables
![image](https://github.com/user-attachments/assets/ba171f7f-40aa-4f88-a924-542db04eacf6)
#### Figure 3: Bar Charts of Binary Variables
![image](https://github.com/user-attachments/assets/38b2ea0b-368b-495d-923f-e64f67600369)  ![image](https://github.com/user-attachments/assets/50cb26fa-ac05-43a3-a71c-7381617b7567)



- Checked for multicollinearity using correlation matrices and VIF values.

### Model Selection
- Built an initial full model with all predictor variables.
- Performed best subset regression and stepwise regression for feature selection.
- Evaluated model assumptions (linearity, homoscedasticity, normality of errors, and independence of errors).

### Validation
- Split data into training (195 observations) and validation (193 observations) sets.
- Compared candidate models based on statistical performance metrics.
- Finalized model based on predictive performance and parsimony.

## Final Model
The final model predicts log-transformed REM sleep duration using the following variables:
- `Exercise frequency` (minimal positive association)
- `Age`
- `Awakenings`
- `Alcohol consumption`
- `Smoking status`

## Results
- Exercise frequency explains only **0.94%** of the variance in REM sleep duration.
- More significant predictors include:
  - **Awakenings (10.34%)**
  - **Alcohol consumption (6.98%)**
  - **Smoking status (4.08%)**
- The final model explains **25.17%** of the variance in REM sleep duration (R² = 0.2517).
#### Figure 4: Final Model Residual Diagnostics
![image](https://github.com/user-attachments/assets/7aa62dce-c591-4d3f-9825-00264c074421)


## Limitations
- Limited to multiple linear regression for the purpose of this assignment
- Self-reported data may introduce bias.
- Limited variability due to categorical input restrictions.
- REM sleep duration was derived rather than directly measured.
- A controlled experiment or longitudinal study could improve accuracy.

## Future Work
- Collect more granular data on workout intensity and type.
- Increase sample size and include multiple observations per individual.
- Explore non-linear models or alternative statistical approaches for better predictive accuracy.
