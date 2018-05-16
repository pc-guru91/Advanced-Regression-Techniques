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

### 1.2 Load the Data
Since both test and train sets contain missing values, let's combine them into a single data frame called Ames.
```
test = read.csv("test.csv", stringsAsFactors = FALSE)  
train = read.csv('train.csv', stringsAsFactors = FALSE)  
Ames = rbind(within(train, rm('Id', 'SalePrice')), within(test, rm('Id')))
```

## 2. Data Cleansing
  ### 2.1 Imputing Missing Values
Imputing N/A may be a bit tricky and time consuming. First, let's discover columns with missing data. We see that there are total of 34 columns with missing data.
  ```
  NAcol = which(colSums(is.na(Ames)) > 0)  
  sort(colSums(sapply(Ames[NAcol], is.na)), decreasing = TRUE)
  ```
  ![missing values](https://user-images.githubusercontent.com/38479244/40046515-0680337c-57e2-11e8-91f4-a253538f7a97.png)


**Below are columns with missing data that can be replaced with either 'None' or '0'**
```
Ames$PoolQC[is.na(Ames$PoolQC)] = "None"  
Ames$MiscFeature[is.na(Ames$MiscFeature)] = "None"
Ames$Alley[is.na(Ames$Alley)]= "None" 
Ames$Fence[is.na(Ames$Fence)] = "None"
Ames$FireplaceQu[is.na(Ames$FireplaceQu)] = "None"  
Ames$GarageFinish[is.na(Ames$GarageFinish)] = "None"
Ames$GarageType[is.na(Ames$GarageType)] = "None"
Ames$GarageQual[is.na(Ames$GarageQual)] = "None"
Ames$GarageCond[is.na(Ames$GarageCond)] = "None"
Ames$GarageArea[2577] = 0 
Ames$GarageCars[2577] = 0  
Ames$BsmtQual[is.na(Ames$BsmtQual)] = "None"
Ames$BsmtCond[is.na(Ames$BsmtCon)] = "None" 
Ames$BsmtExposure[is.na(Ames$BsmtExposure)] = "None"
Ames$BsmtFinType1[is.na(Ames$BsmtFinType1)] = "None"  
Ames$BsmtFinType2[is.na(Ames$BsmtFinType2)] = "None" 
Ames$BsmtFullBath[is.na(Ames$BsmtFullBath)] = 0 
Ames$TotalBsmtSF[is.na(Ames$TotalBsmtSF)] = 0  
Ames$BsmtHalfBath[is.na(Ames$BsmtHalfBath)] = 0  
Ames$BsmtFinSF1[is.na(Ames$BsmtFinSF1)] = 0 
Ames$BsmtFinSF2[is.na(Ames$BsmtFinSF2)] = 0
Ames$BsmtUnfSF[is.na(Ames$BsmtUnfSF)] = 0 
Ames$MasVnrType[is.na(Ames$MasVnrType)] = "None"
Ames$MasVnrArea[is.na(Ames$MasVnrArea)] = 0
...
...
```
**Lot Frontage**

We see two ways to impute the missing values:
1. We can replace it by taking the median per neighborhood.
2. We can replace it by fitting the decision tree. 

The former approach will be used in this case. 
```
for (i in 1:nrow(Ames)){  
  if(is.na(Ames$LotFrontage[i])){  
    Ames$LotFrontage[i] = as.integer(median(Ames$LotFrontage[Ames$Neighborhood==Ames$Neighborhood[i]], 
                                            na.rm = TRUE))  
  }  
}   
```

**GarageYrBlt**

Based on the data, we can see that GarageYrBlt usually matches with the YearBuilt. Fortunately, there's not a single observation that contains missing values on both YearBuilt and GarageYrBlt. Therefore, we will replace the missing values with the YearBuilt values. 
```
Ames$GarageYrBlt[is.na(Ames$GarageYrBlt)] = Ames$YearBuilt[is.na(Ames$GarageYrBlt)]
Ames$GarageYrBlt[2593] = 2007  
Ames$GarageYrBlt[2512] = 1923 
```

  ### 2.2 Encoding Ordinal Variables
  #### The following features display quality factor levels : 

| Categorical Features|
| --------|
| PoolQC  |
|ExterQual |
|HeatingQC |
|KitchenQual|
|FireplaceQu|
|GarageQual|
|GarageCond|
|BsmtQual|
|BsmtCond|


  #### Hypothesis Testing
We performed post hoc analysis such as ANOVA factor and TukeyHSD to validate the ordinality of features that are in question. 
  ```
  Quality = c('Ex'=5, 'Gd'=4, 'TA'=3, 'Fa'=2, 'Po'=1, 'None'=0)  
  Ames$FireplaceQu = as.integer(revalue(Ames$FireplaceQu, Quality))
  ```
  ### 2.3 Encoding Categorical Variables
  ### The following features may contain ordinality:
| Categorical Features|
| --------|
| Alley  |
|Fence |
|MasVnrArea |
|KitchenQual|
|FireplaceQu|
|GarageQual|
|GarageCond|
|BsmtQual|
|BsmtCond|
|OverallQual|
|OverallCond|
  
  ### 2.4 Removing Outliers


  
  
  

## 3. Data Exploration
  ### 3.1 Key Features
  ### 3.2 Correlation Matrix
  ### 3.3 Random Forest Feature Importance
  ### 3.4 Removing Highly Correlated Variables
  
## 4. Feature Engineering
  
## 5. Data Pre-Processing
  ### 5.1 Normalize
  ### 5.2 Standardize
  ### 5.4 Principal Component Analysis
  ### 5.5 One-hot Encoding
  ### 5.6 Dropping Dummy Variables W/Zero Variance
  ### 5.7 Normalizing the Target Variable
 
## 6. Modeling
  ### 6.1 Lasso
  ### 6.2 Ridge
  ### 6.3 Elastic Net
  ### 6.4 XGBoost
  ### 6.5 Simple Average
  ### 6.6 Weighted Average
  ### 6.7 Ensemble
  ### 6.8 Stacking

## 7. Evaluation
  ### 7.1 Choose the Best Model
  ### 7.2 Submit Your Work
 
  ```
  write.csv(..., file = '...csv', row.names = FALSE)
  ```
  To view the full codes on this project, click [here](https://github.com/pc-guru91/AmesHousing/blob/master/Ames%20Analysis.R) and [here](https://github.com/pc-guru91/AmesHousing/blob/master/Ames%20Prediction.R)

 Good luck on your next data science competition! :v:

