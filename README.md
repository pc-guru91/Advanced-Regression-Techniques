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
 ![missingvalues](https://user-images.githubusercontent.com/38479244/40098009-1bbfd966-588e-11e8-9ed7-ab97d31bebc7.png)


**Below are columns with missing data that can be replaced with either "None" or "0"**
```
Ames$PoolQC[is.na(Ames$PoolQC)] = "None"  
Ames$MiscFeature[is.na(Ames$MiscFeature)] = "None"
Ames$Alley[is.na(Ames$Alley)]= "None" 
Ames$Fence[is.na(Ames$Fence)] = "None"
Ames$FireplaceQu[is.na(Ames$FireplaceQu)] = "None"  
Ames$GarageFinish[is.na(Ames$GarageFinish)] = "None"
Ames$GarageType[is.na(Ames$GarageType)] = "None"
Ames$GarageQual[is.na(Ames$GarageQual)] = "None"
Ames$BsmtQual[is.na(Ames$BsmtQual)] = "None"
Ames$BsmtExposure[is.na(Ames$BsmtExposure)] = "None"
Ames$BsmtFinType1[is.na(Ames$BsmtFinType1)] = "None"  
Ames$TotalBsmtSF[is.na(Ames$TotalBsmtSF)] = 0  
Ames$BsmtFinSF1[is.na(Ames$BsmtFinSF1)] = 0 
Ames$BsmtUnfSF[is.na(Ames$BsmtUnfSF)] = 0 
Ames$MasVnrType[is.na(Ames$MasVnrType)] = "None"
Ames$MasVnrArea[is.na(Ames$MasVnrArea)] = 0
...
(Note: Not all features are shown)
```
**Lot Frontage**

There are multiple ways to impute this kind of missing values, let's just define a couple of ways:
1. We can replace it by taking the median per neighborhood.
2. We can replace it by fitting the decision tree. 

![rplot](https://user-images.githubusercontent.com/38479244/40098133-c016873a-588e-11e8-917c-132d67d84995.png)

The former approach will be used because it's evident that lot frontage varies by the type of neighborhood a house is located. However, fitting a decision tree and predicting for missing values can be a reasonable approach as well.  
```
for (i in 1:nrow(Ames)){  
  if(is.na(Ames$LotFrontage[i])){  
    Ames$LotFrontage[i] = as.integer(median(Ames$LotFrontage[Ames$Neighborhood==Ames$Neighborhood[i]], 
                                            na.rm = TRUE))  
  }  
}   
```

**GarageYrBlt**

Based on the data, we find that GarageYrBlt, most of the time, matches with the YearBuilt. Intuitively, this makes sense because when a house is built, a garage is built as well. Therefore, we will replace the missing values with the YearBuilt values. 
```
Ames$GarageYrBlt[is.na(Ames$GarageYrBlt)] = Ames$YearBuilt[is.na(Ames$GarageYrBlt)]
```

**Special Considerations**

```
Ames$MasVnrType[is.na(Ames$MasVnrArea)] = "None"
Ames$GarageYrBlt[2593] = 2007  
Ames$GarageYrBlt[2512] = 1923 
Ames$GarageArea[2577] = 0 
Ames$GarageCars[2577] = 0
Ames$GarageFinish[c(2127,2577)] = names(sort(-table(Ames$GarageFinish)))[[1]]                   #Replacing NA with mode
Ames$GarageQual[c(2127,2577)] = names(sort(-table(Ames$GarageQual)))[[1]]                       #Replacing NA with mode
Ames$GarageCond[c(2127,2577)] = names(sort(-table(Ames$GarageCond)))[[1]]                       #Replacing NA with mode
Ames$BsmtQual[c(2218,2219)] = names(sort(-table(Ames$BsmtQual)))[[1]]                           #Replacing NA with mode
Ames$BsmtCond[c(2041,2186,2525)] = names(sort(-table(Ames$BsmtCond)))[[1]]                      #Replacing NA with mode
Ames$BsmtExposure[c(949, 1488, 2349)] = names(sort(-table(Ames$BsmtExposure)))[[1]]             #Replacing NA with mode
Ames$BsmtFinType2[333] = names(sort(-table(Ames$BsmtFinType2)))[[1]]                            #Replacing NA with mode
Ames$KitchenQual[1556] = names(sort(-table(Ames$KitchenQual[Ames$KitchenAbvGr %in% 1])))[[1]]   #Replacing NA with mode
Ames$Exterior2nd[2152] = names(sort(-table(Ames$Exterior2nd[Ames$YearBuilt %in% 1940])))[[1]]   #Replacing NA with mode
Ames$SaleType[2490] = names(sort(-table(Ames$SaleType[Ames$SaleCondition %in% 'Normal'])))[[1]] #Replacing NA with mode
Ames$MSZoning[c(2217, 2905)] = names(sort(-table(Ames$MSZoning[Ames$MSSubClass %in% 20])))[[1]] #Replacing NA with mode  
Ames$MSZoning[1916] = names(sort(-table(Ames$MSZoning[Ames$MSSubClass %in% 30])))[[1]]          #Replacing NA with mode
Ames$MSZoning[2251] = names(sort(-table(Ames$MSZoning[Ames$MSSubClass %in% 70])))[[1]]          #Replacing NA with mode
Ames$Functional[is.na(Ames$Functional)] = names(sort(-table(Ames$Functional)))[[1]]             #Replacing NA with mode
Ames$Electrical[1380] = names(sort(-table(Ames$Electrical[Ames$Heating %in% 'GasA'])))[[1]]     #Replacing NA with mode
```
  ### 2.2 Encoding Ordinal Variables
  #### The following features display quality factor levels : 

| Categorical Features|
| --------|
| PoolQC  |
|ExterQual|
|ExterCond|
|HeatingQC|
|KitchenQual|
|FireplaceQu|
|GarageQual|
|GarageCond|
|BsmtQual|
|BsmtCond|


  ```
  Quality = c('Ex'=5, 'Gd'=4, 'TA'=3, 'Fa'=2, 'Po'=1, 'None'=0)  
  Ames$FireplaceQu = as.integer(revalue(Ames$FireplaceQu, Quality))
  ```
  ### 2.3 Encoding Categorical Variables
  ### The following features may contain ordinality:
| Categorical Features|
| --------|
| Alley  |
|MasVnrType|
|FireplaceQu|
|GarageFinish|
|BldgType|
|PavedDrive|
|CentralAir|
|MSZoning|
|Street|

#### Hypothesis Testing
We will perform post hoc analysis using both ANOVA factor and TukeyHSD to validate the ordinality of features in question. 
 ```
 fit = aov(SalePrice ~ Alley)
 TukeyHSD(fit)
 ----------------------------------------------------
               diff      lwr      upr p adj
Pave-Grvl 45781.51 30527.29 61035.72     0
 ```
 ```
 fit = aov(SalePrice ~ Fence)
 TukeyHSD(fit)
 ----------------------------------------------------
                   diff       lwr       upr     p adj
GdWo-GdPrv  -38548.143 -68163.46 -8932.826 0.0048296
MnPrv-GdPrv -30176.368 -54189.63 -6163.110 0.0071060
MnWw-GdPrv  -44641.094 -96285.91  7003.721 0.1166308
MnPrv-GdWo    8371.774 -16436.39 33179.935 0.8192201
MnWw-GdWo    -6092.951 -58112.13 45926.230 0.9903469
MnWw-MnPrv  -14464.726 -63511.28 34581.831 0.8713228
 ```

 ### 2.4 


  
  
  

## 3. Data Exploration
  ### 3.1 Key Features
  ### 3.2 Correlation Matrix
  ### 3.3 Random Forest Feature Importance
  ### 3.4 Removing Highly Correlated Variables
  
## 4. Feature Engineering
  
  
## 5. Data Pre-Processing
  ### 5.1 Removing Outliers
  ### 5.2 Normalize
  ### 5.3 Standardize
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

