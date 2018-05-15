## AmesHousing

![ames](https://user-images.githubusercontent.com/38479244/40045829-35eaef00-57e0-11e8-8600-f4fee1b5ad1a.jpg)

## 1. Executive Summary
Ask a home buyer to describe their dream house, and they probably won't begin with the height of the basement ceiling or the proximity to an east-west railroad. But this playground competition's dataset proves that much more influences price negotiations than the number of bedrooms or a white-picket fence.

With 79 explanatory variables describing (almost) every aspect of residential homes in Ames, Iowa, this competition challenges you to predict the final price of each home.

### 1.1 Brief Introduction
The dataset (both training and testing) is provided by [Kaggle](https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data). All features fall under one of these categories:

- Age variables
- Basement variables
- Garage variables
- Kitchen variables
- Lot/land variables
- Location variables
- Outdoor features (porches, misc, etc.) variables
- Pool variables
- Room/bathroom variables
- Roof variables
- Utilities variables

For additional details, click the [link](https://ww2.amstat.org/publications/jse/v19n3/decock/DataDocumentation.txt).

## 2. Data Cleansing
  ### 2.1 Imputing Missing Values
   Imputing N/A may be a bit tricky and time consuming. First, let's write codes below to identify columns with missing data. We see that there are total of 34 columns with missing data. 
  ```
  NAcol = which(colSums(is.na(Ames)) > 0)  
  sort(colSums(sapply(Ames[NAcol], is.na)), decreasing = TRUE)
  ```
  ![missing values](https://user-images.githubusercontent.com/38479244/40046515-0680337c-57e2-11e8-91f4-a253538f7a97.png)

  ### 2.2 Converting Categorical to Ordinal
  #### Features with quality factor levels are as follows: 
Pool QC, Exter Qual, Exter Cond, HeatingQC, KitchenQual, FireplaceQu, Garage Qual, Garage Cond, Bsmt Qual, Bsmt Cond, OverallQual, and OverallCond. 
  #### Hypothesis Testing
We performed post hoc analysis such as ANOVA factor and TukeyHSD to validate the ordinality of features that are in question. 
  ```
  Quality = c('Ex'=5, 'Gd'=4, 'TA'=3, 'Fa'=2, 'Po'=1, 'None'=0)  
  Ames$FireplaceQu = as.integer(revalue(Ames$FireplaceQu, Quality))
  ```
  ### 2.3 Removing Outliers


  
  
  

## 3. Data Exploration
  ### 3.1 Key Features
  ### 3.2 Correlation Matrix
  ### 3.3 Random Forest Feature Importance
  ### 3.4 Removing Highly Correlated Variables
  
## 4. Data Pre-Processing
  ### 4.1 Normalize
  ### 4.2 Standardize
  ### 4.3 Principal Component Analysis
  ### 4.3 One-hot Encoding
  ### 4.4 Dropping Dummy Variables W/Zero Variance
  ### 4.5 Normalizing the Target Variable
 
## 5. Modeling
  ### 5.1 Lasso
  ### 5.2 Ridge
  ### 5.3 Elastic Net
  ### 5.4 XGBoost
  ### 5.5 Simple Average
  ### 5.6 Weighted Average
  ### 5.7 Ensemble
  ### 5.8 Stacking

## 6. Evaluation
  ### 6.1 Choose the Best Model
  ### 6.2 Submit Your Work
  Good luck on your next data science competition! :v:



```
