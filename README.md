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
Notice that the adjusted p-value is 0 indicating that significant means exist between different groups of a feature. Therefore, we will acknowledge that ordinality exists and assign label encoding. 
```
Alley = c('None'=0, 'Grvl'=1, 'Pave'=2)
Ames$Alley = as.integer(revalue(Ames$Alley, Alley)) 

Garage = c('Fin'=3, 'RFn'=2, 'Unf'=1, 'None'=0)  
Ames$GarageFinish = as.integer(revalue(Ames$GarageFinish, Garage)) 
...
(Note: Not all features are explained, but recognize that this analysis will be performed consistently to the features and determine which can be transformed with label encoding.
```
#### After testing all categorical features, we found that following features contain ordinality:
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
 The MSSubClass should be factorized. Otherwise, MSSubClass groups can mislead MSSubClass = 190 to have the highest weight and MSSubClass = 20 to have the lowest weight in predicting for the SalePrice. 
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
 Both features are proned to seasonality with a hint of cyclicality. The SalePrice varies by different months and years in which homes are sold. 
 
![mo yr sold](https://user-images.githubusercontent.com/38479244/40102807-51fe7fde-58a0-11e8-9de3-516723372641.png)

```
Ames$MoSold = as.factor(Ames$MoSold)
Ames$YrSold = as.factor(Ames$YrSold)
```  
### 2.5 Factorizing the rest of categorical variables
Now that we've imputed all missing values, we will begin to factorize all of the rest of categorical variables. Here's a list:
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
  Based on correlation matrix and random forest variance importance, we can narrow down all 79 variables to 5 main variables that affect   the SalePrice the most:
   
   1. OverallQual
   2. GrLivArea
   3. Neighborhood
   4. YearBuilt
   5. MSSubClass
   
  #### OverallQual
  Overall quality of a house has a scale ranging from 1 (poor) to 10 (very excellent). Based on the observation, there's a significant difference between each group of the feature in relation to SalePrice. 
  
  ![overallqual](https://user-images.githubusercontent.com/38479244/40111812-dbe39c8a-58b8-11e8-85ad-82443885a326.png)
 
 #### GrLivArea.
  There are some outliers present. We will address them later. For now, we will identify that positive relationship exists between GrLivArea and SalePrice. 
  
  ![final grlivarea outliers](https://user-images.githubusercontent.com/38479244/40148248-eb20b566-5921-11e8-9a2a-8f868220b0eb.png)

#### YearBuilt
  The growing trends across the timeline is evident. However, cyclicality is found meaning SalePrice is affected by business cycles and economic upturns and downturns, which are volatile and extremely difficult to predict. In this case, YearBuilt shows a more defined cyclical trend than that of YearRemodel and GarageYrBlt.
  
![final time series](https://user-images.githubusercontent.com/38479244/40148152-72a36598-5921-11e8-882f-ccb056a7d6d0.png)

 #### Neighborhood
  Each neighborhood group is not evenly distributed, for example, "Blueste" and "NPkVill" have less than 10 observations in the training dataset. For simplicity, we will proceed assuming that these two neighborhoods represent the "true" value of the SalePrice. Depending on which neighborhood a house is located in, the SalePrice can differ approximately from $245,000 to $25,000. Possibly, neighborhood groups can be binned in relation to the magnitude of SalePrice. 
  
  ![neighborhood](https://user-images.githubusercontent.com/38479244/40111838-ee70a3ac-58b8-11e8-8b11-274cb7c31e2d.png)
  
 #### MSSubClass
  The boxplot chart is arranged in ascending order in terms of median SalePrice. This emphasizes homes that were built 1946 & newer relatively have high median Saleprice. Also, the outliers are labeled as red colored points and extreme ones are specificlally labeled with the rowname of the dataset. 
  
  ![mssubclass](https://user-images.githubusercontent.com/38479244/40147772-e6d969d2-591f-11e8-8a3d-3c1106353ef1.png)
  
       - 020	1-STORY 1946 & NEWER ALL STYLES       
       - 030	1-STORY 1945 & OLDER
       - 040	1-STORY W/FINISHED ATTIC ALL AGES
       - 045	1-1/2 STORY - UNFINISHED ALL AGES
       - 050	1-1/2 STORY FINISHED ALL AGES
       - 060	2-STORY 1946 & NEWER
       - 070	2-STORY 1945 & OLDER
       - 075	2-1/2 STORY ALL AGES
       - 080	SPLIT OR MULTI-LEVEL
       - 085	SPLIT FOYER
       - 090	DUPLEX - ALL STYLES AND AGES
       - 120	1-STORY PUD (Planned Unit Development) - 1946 & NEWER
       - 150	1-1/2 STORY PUD - ALL AGES
       - 160	2-STORY PUD - 1946 & NEWER
       - 180	PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
       - 190	2 FAMILY CONVERSION - ALL STYLES AND AGES
  
## 4. Feature Engineering
This section is critical in optimizing predictive models. We will create stronger input variables that would explain more information about the dataset than the ones we already have. In order to do this, the user is required to have domain knowledge.

 ### 4.1 Feature engineering on numeric data
So far, we covered that OverallQual, GrLivArea, and YearBuilt have the one of the highest correlations with SalePrice. Since both living space and quality of a house are important we will look more closely on these components in order to create a new feature that would explain more about the data. 
 ```
# TotalBsmtSF = BsmtFinSF + BsmtUnfSF
# GrLivArea   = 1stFlrSF + 2ndFlrSF + LowQualFinSF 
# TotalArea = TotalBsmtSF + GrLivArea
Ames = Ames %>% mutate(TotalArea = TotalBsmtSF+GrLivArea)  
Ames$TotalArea = as.integer(Ames$TotalArea)  

#Total Outdoor Space
Ames = Ames %>% mutate(TotalOutdoorSF = OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch)  
Ames$TotalOutdoorSF = as.integer(Ames$TotalOutdoorSF) 

#Total bathrooms in the house
Ames = Ames %>% mutate(TotalBaths = BsmtFullBath + BsmtHalfBath*(0.5) + FullBath + HalfBath*(0.5))  
Ames$TotalBaths = as.integer(Ames$TotalBaths) 
```
# 

![house new](https://user-images.githubusercontent.com/38479244/40149272-a4f0a10e-5927-11e8-91d6-87e6da1209cf.png)

```
#Is it a new house?
Ames$HouseNew = ifelse(Ames$YrSold == Ames$YearBuilt, 1, 0)
Ames$HouseNew = as.integer(Ames$HouseNew)
```

![remodelled](https://user-images.githubusercontent.com/38479244/40149274-a6447120-5927-11e8-9adc-a3ba7928c30c.png)

```
#Is it remodelled?
Ames$Remodelled = ifelse(Ames$YearRemodAdd != Ames$YearBuilt, 0, 1)
Ames$Remodelled = as.integer(Ames$Remodelled)
 ```

 ### 4.2 Feature engineering on categoric data
 We can try binning approach with Neighborhood and MSSubClass by subdividing them into equal intervals of SalePrice. 
 ```
 #Binning Neighborhood
 nbrh.map <- c('MeadowV' = 0, 'IDOTRR' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1,   
              'BrkSide' = 1, 'Sawyer' =2, 'Blueste' = 2, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,  
              'SawyerW' = 3, 'Gilbert' = 3, 'NWAmes' = 3, 'Blmngtn' = 3, 'CollgCr' = 3, 'ClearCr' = 3,   
              'Crawfor' = 3, 'Veenker' = 4, 'Somerst' = 4, 'Timber' = 4, 'StoneBr' = 5, 'NoRidge' = 5,   
              'NridgHt' = 5)  
Ames['NeighborhoodBin'] = as.numeric(nbrh.map[Ames$Neighborhood])

 ```

## 5. Data Pre-Processing
  ### 5.1 Removing highly correlated variables
Multicollinearity is one of the issues in producing poor results for applied machine learning. The correlation will be set at cutoff = 0.75 to decide at which highly correlated features to remove. 
  ```
numericVar = which(sapply(Ames.train, is.numeric))  
Ames_numVar = Ames.train[, numericVar]
Cor_numVar = cor(Ames_numVar, use = "pairwise.complete.obs")  
Cor_numVar[is.na(Cor_numVar)] = 0
findCorrelation(Cor_numVar, cutoff = 0.75)
------------------------------------------------
[1] "GarageCars" "YearBuilt" "TotalBsmtSF" "GrLivArea" "FireplaceQu" "GarageCond" "PavedDrive" "PoolQC"     
[9] "PoolArea"
  ```
Before we remove the features it's important to refer back to the correlation matrix and random forest variable importance. We will remove one of two highly correlated features that has slightly lower correlations with a response variable. The final list would be:

[1] "GarageArea" "YearRemodAdd" "TotalBsmtSF" "FireplaceQu" "GarageCond" "PavedDrive" "Pool" "PoolArea" "TotRmsAbvGrd"  
 
  ### 5.2 Removing outliers
  We found four extreme outliers in GrLivArea and MSSubClass. Although removing outliers can be dangerous, we will remove them because they skew the data and do not follow the overall trend of the data. 
  ```
  Ames = Ames[-c(524,692,1183,1299), ]
  ```
  ### 5.3 Skewness and kurtosis of predictors
  One of the important assumptions for regression models is normality. In order to preserve this assumption, a very common method of log transformation will be applied to all continuous predictors.  
  ```
  numVar = which(sapply(Ames, is.numeric))
  Ames_numVar = Ames[, numVar]
  true_numVar = c('LotFrontage', 'LotArea', 'YearBuilt','MasVnrArea','BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF','X1stFlrSF',                                 'LowQualFinSF', 'X2ndFlrSF','GrLivArea', 'GarageYrBlt','WoodDeckSF', 'OpenPorchSF',                                                     'EnclosedPorch','X3SsnPorch','ScreenPorch', 'MiscVal', 'TotalArea','TotalOutdoorSF')
  true_numVar = Ames_numVar[, true_numVar]
 
 for(i in 1:ncol(true_numVar)){
  if(abs(skew(true_numVar [, i]))>0.7){
    true_numVar[, i] = log(true_numVar[, i]+1)
  }
}
  ```
  ### 5.4 Standardize
  Both training and testing sets need to be scaled and centered to have mean = 0 and standard deviation of 1. 
  ```
**#Standardize training set**
scale_numtrain = Ames_numVar[1:1456, colnames(true_numVar)]
scale.train = preProcess(scale_numtrain, method = c("center", "scale"))
scale.train = predict(scale.train, scale_numtrain)
**#Standardize testing set**
scale_numtest = Ames_numVar[1457:nrow(Ames_numVar), colnames(true_numVar)]
scale.test = preProcess(scale_numtest, method = c("center", "scale"))
scale.test = predict(scale.test, scale_numtest)

  ```
  ### 5.5 Principal component analysis
This is one of the algorithms that can reduce dimensions but still preserve variance within the data. Generally, PCA calculates linear combinations of predictors 
```
pr.out = prcomp(scale.train, scale. = F)    
summary(pr.out)                             #Reduced 4 dimensions and still containing 100% of information
pr.data = pr.out$x

pr.train.data = data.frame(pr.data, SalePrice = train$SalePrice)
pr.train.data = pr.train.data[, 1:19]
pr.test.data = predict(pr.out, newdata = scale.test)
pr.test.data = data.frame(pr.test.data, SalePrice = final.test$SalePrice)
pr.test.data = pr.test.data[, 1:19]
```
Unfortunately, the PCA did not improve result for this dataset. We will proceed without using it. 

  ### 5.6 One-hot encoding
The regression models require all variables to be numeric. Therefore, we need to convert all categorical variables by performing one-hot encoding. 
 ```
Ames_catVar = Ames[, -numVar]
dfdummies = as.data.frame(model.matrix(~.-1, Ames_catVar))
dummies.train = dfdummies[1:1456, ]
dummies.test = dfdummies[1457:nrow(dfdummies), ]
 ```
  ### 5.7 Dropping dummy variables w/zero variance
We will drop dummy variables with zero variance because these features have zero observations and convey no information of the data. We can consider dropping dummy variables with near zero variance but it would cause too much information to be lost. 

```
nzv.train = nearZeroVar(final.train, saveMetrics = TRUE)
drop.cols1 = rownames(nzv.train)[nzv.train$zeroVar == TRUE]
```

  ### 5.8 Normalizing the target variable
This is the final step before we can implement the models. Just as the predictors, the target variable should be normalized to follow the assumption of regression models. We recommend using simple transformation method for this can benefit the interpretability of a model. 
```
log(final.train$SalePrice) %>% skew() # 0.0653 meets the -0.8 to 0.8 threshold
log(final.train$SalePrice) %>% kurtosi() #0.655 meets the -3.0 to 3.0 threshold
final.train$SalePrice = log(final.train$SalePrice) 
```
Now that we have log transformed the response variable it looks it's apprixmating normal distribution, so we will proceed. 

![normality of response var](https://user-images.githubusercontent.com/38479244/40151299-eda2ab02-5933-11e8-9c69-5c0240a56890.png)

## 6. Modeling
  ### 6.1 Lasso regression
 
 ```
#a: Set controls for Lasso
set.seed(102091)
control = trainControl(method = "repeatedcv", number = 10, repeats = 2)
lasso_grid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by = 0.0005))

#b: Fit Lasso
lasso.fit = train(SalePrice~., data = final.train, method = 'glmnet', trControl = control,
                  tuneGrid = lasso_grid)
#c: Predict Lasso
pred.lasso = predict(lasso.fit, final.test)
prediction.lasso = data.frame(Id = seq(1461, 2919, 1), SalePrice = exp(pred.lasso))
  ```
**(Note that the predicted values need to be exp( ) to bring back the original values because log transformation was performed on the target variable.)**

  ### 6.2 Ridge regression
  ```
set.seed(102091)
ridge_grid = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.0005))
#a: Fit Ridge
ridge.fit = train(SalePrice~., data = final.train, method = 'glmnet', trControl = control,
                   tuneGrid = ridge_grid)
#b: Predict Ridge
pred.ridge = predict(ridge.fit, final.test)
prediction.ridge = data.frame(Id = seq(1461, 2919, 1), SalePrice = exp(pred.ridge))
  ```
  ### 6.3 Elastic net
```
set.seed(102091)
elastic_grid = expand.grid(alpha = 0.5, lambda = seq(0.001, 0.1, by = 0.0005))
#a: Fit Elastic Net
elastic.fit = train(SalePrice~., data = final.train, method = 'glmnet', trControl = control,
                   tuneGrid = elastic_grid)
#b: Predict Elastic Net
pred.elastic = predict(elastic.fit, final.test)
prediction.elastic = data.frame(Id = seq(1461, 2919, 1), SalePrice = exp(pred.elastic))
```
  ### 6.4 XGBoost
```
#a: Set controls for XGBoost----------------------------------------------------------------------
set.seed(102091)
cv.control = trainControl(method = "repeatedcv", number = 10, repeats = 2, allowParallel = TRUE)
XGBoost_grid = expand.grid(gamma = 0, nrounds = 500, max_depth=seq(2,8,1), eta = c(0.1, 0.05, 0.01), colsample_bytree=1, 
                           subsample=1, min_child_weight=c(1,2,3,4,5))
xgb_grid_opt = expand.grid(objective = 'reg:linear', booster = 'gbtree', gamma = 0, nrounds = 500, max_depth=3, eta = 0.05, 
                           colsample_bytree=1, subsample=1, min_child_weight=4)
#b: Tune & Fit XGBoost
XGBoost.fit = train(SalePrice~., data = final.train, method = 'xgbTree', trControl = cv.control,
                    tuneGrid = XGBoost_grid, metric = "RMSE")
#c: Predict XGBoost
pred.xgb = predict(XGBoost.fit, final.test)
prediction.xgb = data.frame(Id = seq(1461, 2919, 1), SalePrice = exp(pred.xgb))

```
  ### 6.5 Simple average
```
pred.s_average = (prediction.elastic$SalePrice+prediction.ridge$SalePrice+prediction.lasso$SalePrice)/3
predictions.s_average = data.frame(Id = seq(1461, 2919, 1), SalePrice = pred.s_average)

```
  ### 6.6 Weighted average
```
pred.w_average = (prediction.elastic$SalePrice+prediction.ridge$SalePrice+prediction.lasso$SalePrice)/3
predictions.w_average = data.frame(Id = seq(1461, 2919, 1), SalePrice = pred.w_average)
```
  ### 6.7 Ensemble
```
# Weighted Average Ensemble -------------------------------------------------------
#a: Set controls
set.seed(102091)
Gridlasso = expand.grid(.alpha = 1, .lambda = seq(0.001,0.1,by = 0.001))
Gridridge = expand.grid(.alpha = 0, .lambda = seq(0.001,0.1,by = 0.001))
Gridelastic = expand.grid(.alpha = 0.5, .lambda = seq(0.001,0.1,by = 0.001))
trcontrol = trainControl(method = "cv", number = 10, savePredictions = 'final', allowParallel = TRUE)

modelList2 = caretList(SalePrice~., data = final.train, trControl=trcontrol, metric="RMSE", tuneList = list(
  glmnet = caretModelSpec(method = "glmnet", tuneGrid = Gridlasso),
  glmnet = caretModelSpec(method = "glmnet", tuneGrid = Gridridge),
  glmnet = caretModelSpec(method = "glmnet", tuneGrid = Gridelastic)
))

#b: Fit Ensemble
Ensemble = caretEnsemble(modelList2, metric = "RMSE", trControl = control)

#c: Predict Ensemble
pred.ensemble = predict(Ensemble, final.test)
prediction.ensemble = data.frame(Id = seq(1461, 2919, 1), SalePrice = exp(pred.ensemble))
```

## 7. Submitting Work
  Usually, ensemble methods would produce the highest accurary. 
  ```
  write.csv(..., file = '....csv', row.names = FALSE)
  ```
  To view the full codes on this project, click [Part 1](https://github.com/pc-guru91/AmesHousing/blob/master/Ames%20Analysis.R) and [Part 2](https://github.com/pc-guru91/AmesHousing/blob/master/Ames%20Prediction.R)

 **Good luck on your next data science competition!** :v:

