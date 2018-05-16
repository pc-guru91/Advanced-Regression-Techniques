## AmesHousing

![ames](https://user-images.githubusercontent.com/38479244/40045829-35eaef00-57e0-11e8-8600-f4fee1b5ad1a.jpg)

## 1. Executive Summary
Ask a home buyer to describe their dream house, and they probably won't begin with the height of the basement ceiling or the proximity to an east-west railroad. But this playground competition's dataset proves that much more influences price negotiations than the number of bedrooms or a white-picket fence.

With 79 explanatory variables describing (almost) every aspect of residential homes in Ames, Iowa, this competition challenges you to predict the final price of each home.

### 1.1 Brief introduction
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

**For additional details, click the [link](https://ww2.amstat.org/publications/jse/v19n3/decock/DataDocumentation.txt).**

### 1.2 Load the dataset
Since both test and train sets contain missing values, let's combine them into a single data frame called Ames.
```
test = read.csv("test.csv", stringsAsFactors = FALSE)  
train = read.csv('train.csv', stringsAsFactors = FALSE)  
Ames = rbind(within(train, rm('Id', 'SalePrice')), within(test, rm('Id')))
```

## 2. Data Cleansing
  ### 2.1 Imputing missing values
Imputing N/A may be a bit tricky and time consuming. First, let's discover columns with missing data. We see that there are total of 34 columns with missing data.
  ```
  NAcol = which(colSums(is.na(Ames)) > 0)  
  sort(colSums(sapply(Ames[NAcol], is.na)), decreasing = TRUE)
  ```
 ![missingvalues](https://user-images.githubusercontent.com/38479244/40098009-1bbfd966-588e-11e8-9ed7-ab97d31bebc7.png)


#### Below are columns with missing data that can be replaced with either "None" or "0"
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
#### Lot Frontage

There are multiple ways to impute this type of missing values, let's just define a couple of ways:
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

#### GarageYrBlt

Based on the data, we find that GarageYrBlt, most of the time, matches with the YearBuilt. Intuitively, this makes sense because when a house is built, a garage is built as well. Therefore, we will replace the missing values with the YearBuilt values. 
```
Ames$GarageYrBlt[is.na(Ames$GarageYrBlt)] = Ames$YearBuilt[is.na(Ames$GarageYrBlt)]
```

#### Special Considerations

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
  ### 2.2 Encoding ordinal variables
  #### The following 10 features display quality factor levels : 
  >BsmtQual, BsmtCond, ExterQual, ExterCond, HeatingQC, KitchenQual, FireplaceQu, GarageQual, GarageCond, PoolQC

![quality plots](https://user-images.githubusercontent.com/38479244/40100687-b6bf3230-5899-11e8-8606-e725435bfea3.png)

**According to the plots above, we can conclude that there's a natural ordering in features relative to median of the target variable ('Ex'< 'Gd' < 'Ta' < 'Fa' < 'Po').**  
  ```
  Quality = c('Ex'=5, 'Gd'=4, 'TA'=3, 'Fa'=2, 'Po'=1, 'None'=0)  
  Ames$FireplaceQu = as.integer(revalue(Ames$FireplaceQu, Quality))
  ...
  (Note: Repeat this process for all other 9 features w/quality levels)
  ```
  ### 2.3 Encoding categorical variables
#### Hypothesis Testing
We will perform post hoc analysis using both ANOVA factor and TukeyHSD to validate the ordinality of features in question. To give a couple of examples of how this works, refer to the descriptions below:
 ```
 fit = aov(SalePrice ~ Alley)
 TukeyHSD(fit)
 ----------------------------------------------------
               diff      lwr      upr p adj
Pave-Grvl 45781.51 30527.29 61035.72     0
 ```
 ```
 fit = aov(SalePrice ~ GarageFinish)
 TukeyHSD(fit)
 ----------------------------------------------------
             diff        lwr       upr p adj
RFn-Fin -38370.55  -49852.72 -26888.38     0
Unf-Fin -98283.00 -108948.85 -87617.14     0
Unf-RFn -59912.45  -69985.40 -49839.49     0
 ```
Notice that the adjusted p-value is 0, indicating that significant means exist between different groups of a feature. Therefore, we will acknowledge that ordinality exists and assign label encoding. 
```
Alley = c('None'=0, 'Grvl'=1, 'Pave'=2)
Ames$Alley = as.integer(revalue(Ames$Alley, Alley)) 

Garage = c('Fin'=3, 'RFn'=2, 'Unf'=1, 'None'=0)  
Ames$GarageFinish = as.integer(revalue(Ames$GarageFinish, Garage)) 
...
(Note: Not all features are explained, but recognize that this analysis will be performed consistently to the features and determine which can be transformed with label encoding.
```
#### After testing all categorical features, we find that the following features contain ordinality:
| Categorical Features|
| --------|
| Alley  |
|BldgType|
|CentralAir|
|GarageFinish|
|MasVnrType|
|MSZoning|
|PavedDrive|
|Street|

 ### 2.4 Factorizing numeric variables
 #### MSSubClass
 The MSSubClass should be factorized. Otherwise, the integers can interfere with machine learning algorithms to recognize that MSSubClass = 190 to have the highest weight in predicting for SalePrice, which can be misleading and produce wrong results. 
 ```
    MSSubClass median count
        <int>  <dbl>  <int>
 1        180  88500    10
 2         30  99900    69
 3         45 107500    12
 4        190 128250    30
 5         50 132000   144
 6         90 135980    52
 7         85 140750    20
 8         40 142500     4
 9        160 146000    63
10         70 156000    60
11         20 159250   536
12         75 163500    16
13         80 166500    58
14        120 192000    87
15         60 216000   297

Ames$MSSubClass=as.factor(Ames$MSSubClass)
```

 #### MoSold & YrSold
 Both features are proned to seasonality with a hint of cyclicality and SalePrice varies by the months and years in which homes are sold. 
 
![mo yr sold](https://user-images.githubusercontent.com/38479244/40102807-51fe7fde-58a0-11e8-9de3-516723372641.png)

```
Ames$MoSold = as.factor(Ames$MoSold)
Ames$YrSold = as.factor(Ames$YrSold)
```  
### 2.5 Factorizing the rest of categorical variables
Now that we imputed all missing values, we will begin to factorize all of the rest of categorical variables. Here's a list:
```
Ames$MiscFeature = as.factor(Ames$MiscFeature)        |     Ames$Functional = as.factor(Ames$Functional)
Ames$Fence = as.factor(Ames$Fence)                    |     Ames$Electrical = as.factor(Ames$Electrical)
Ames$GarageType = as.factor(Ames$GarageType)          |     Ames$LandSlope = as.factor(Ames$LandSlope)
Ames$BsmtFinType1 = as.factor(Ames$BsmtFinType1)      |     Ames$LandContour = as.factor(Ames$LandContour)
Ames$BsmtFinType2 = as.factor(Ames$BsmtFinType2)      |     Ames$LotConfig = as.factor(Ames$LotConfig)
Ames$SaleType = as.factor(Ames$SaleType)              |     Ames$LotShape = as.factor(Ames$LotShape)
Ames$Exterior1st = as.factor(Ames$Exterior1st)        |     Ames$Condition1 = as.factor(Ames$Condition1)
Ames$Exterior2nd = as.factor(Ames$Exterior2nd)        |     Ames$Condition2 = as.factor(Ames$Condition2)
Ames$HouseStyle = as.factor(Ames$HouseStyle)          |     Ames$RoofMatl = as.factor(Ames$RoofMatl)
Ames$RoofStyle = as.factor(Ames$RoofStyle)            |     Ames$Heating = as.factor(Ames$Heating)
Ames$Foundation = as.factor(Ames$Foundation)          |     Ames$SaleType = as.factor(Ames$SaleType)
Ames$SaleCondition = as.factor(Ames$SaleCondition) 
```

## 3. Data Exploration
   
  ### 3.1 Correlation matrix of numerical variables
  
  ![correlation matrix](https://user-images.githubusercontent.com/38479244/40106818-a1b5d5d0-58ab-11e8-872b-c69eab93c72e.png)
  
  ### 3.2 Random Forest variance importance
  
![randomforest importance](https://user-images.githubusercontent.com/38479244/40110140-162616e8-58b4-11e8-9ab6-f5d0c18878db.png)

  ### 3.3 Key Features
  Based on correlation matrix and random forest variance importance, we can narrow down all 79 variables to 4 main variables that affect   the SalePrice the most:
   
   1. OverallQual
   2. GrLivArea
   3. Neighborhood
   4. YearBuilt
   
  #### OverallQual
  Overall quality of a house has a scale ranging from 1 (poor) to 10 (very excellent). Based on the observation, there's a significant difference between each group of the feature in relation to SalePrice. 
  
  ![overallqual](https://user-images.githubusercontent.com/38479244/40111812-dbe39c8a-58b8-11e8-85ad-82443885a326.png)
 
 #### GrLivArea.
  There are some outliers present. We will address them later. For now, we will identify that positive relationship exists between GrLivArea and SalePrice. 
  
  ![grlivarea](https://user-images.githubusercontent.com/38479244/40111819-e1c9efd2-58b8-11e8-93b4-d58eb031a81d.png)

#### YearBuilt
  The growing trends across the timeline is evident. However, cyclicality is present, meaning SalePrice is affected by business cycles and economic upturns and downturns, which are volatile and extremely difficult to predict. In this case, it seems for YearBuilt to show a more defined cyclical trend than that of YearRemodel.
  
  ![yearbulit yearremod](https://user-images.githubusercontent.com/38479244/40111833-e8d73df2-58b8-11e8-9798-55642f8b5b1e.png)
 
 #### Neighborhood
  Each neighborhood group is not evenly distributed, for example, "Blueste" and "NPkVill" have less than 10 observations in the training dataset. For simplicity, we will proceed assuming that these two neighborhoods represent the "true" value of the SalePrice. Depending on which neighborhood a house is located in, the SalePrice can differ approximately from $245,000 to $25,000. Possibly, neighborhood groups can be binned in relation to the magnitude of SalePrice. 
  
  ![neighborhood](https://user-images.githubusercontent.com/38479244/40111838-ee70a3ac-58b8-11e8-8b11-274cb7c31e2d.png)
  
## 4. Feature Engineering
This section is crucial to machine learning. Now, we will figure out how we can discover stronger variables that would explain more information about the dataset than the ones we already have. First, we will try to build new features 
  
## 5. Data Pre-Processing
  ### 5.1 Removing highly correlated variables
  ### 5.2 Removing outliers
  ### 5.3 Skewness and kurtosis of predictors
  ### 5.4 Standardize
  ### 5.5 Principal component analysis
  ### 5.6 One-hot encoding
  ### 5.7 Dropping dummy variables w/zero variance
  ### 5.8 Normalizing the target variable
 
## 6. Modeling
  ### 6.1 Lasso regression 
  ### 6.2 Ridge regression
  ### 6.3 Elastic net
  ### 6.4 XGBoost
  ### 6.5 Simple average
  ### 6.6 Weighted average
  ### 6.7 Ensemble
  ### 6.8 Stacking

## 7. Evaluation
  ### 7.1 Choosing the best model
  ### 7.2 Submitting work
 
  ```
  write.csv(..., file = '...csv', row.names = FALSE)
  ```
  To view the full codes on this project, click [here](https://github.com/pc-guru91/AmesHousing/blob/master/Ames%20Analysis.R) and [here](https://github.com/pc-guru91/AmesHousing/blob/master/Ames%20Prediction.R)

 Good luck on your next data science competition! :v:

