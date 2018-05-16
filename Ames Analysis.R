# Requirement -------------------------------------------------------------  
library(tidyverse)  #Wrangling/Manipulation  
library(ggplot2)    #Exploration/Visualization 
library(ggrepel)    #Exploration/Visualization  
library(corrplot)   #Correlation plots  
library(h2o)  
library(plyr)       #Revalue function  
library(glmnet)  
library(randomForest)  
library(rpart)  
library(dplyr)  
library(gridExtra)  
library(forecast)
detach("package:ggplot2")  



# Load the dataset --------------------------------------------------------  
test = read.csv("test.csv", stringsAsFactors = FALSE)  
train = read.csv('train.csv', stringsAsFactors = FALSE)  
Ames = rbind(within(train, rm('Id', 'SalePrice')), within(test, rm('Id')))  
train = train[-c(524,1299),] #Updating for outliers
Ames.train = Ames[1:1458, ]  
Ames.train$SalePrice = train$SalePrice




# # Identify all columns with missing or NA values: ----------------------------------------------------------  
NAcol = which(colSums(is.na(Ames)) > 0)  
a=sort(colSums(sapply(Ames[NAcol], is.na)), decreasing = TRUE) %>% data.frame(Number.of.missing.values=.)  
grid.table(a)  
#Response: Total of 34 missing columns  



# Data Cleansing: "Pool QC" Column -------------------------------------------  
Ames$PoolQC[is.na(Ames$PoolQC)] = "None"  
Ames.train %>% group_by(PoolQC) %>% summarise(median = median(SalePrice), count = n())%>% arrange(median)  
Ames$Pool = ifelse(Ames$PoolArea > 0, 1, 0)
Rate = c('None' = 0, 'Gd'=1, 'Fa'=2, 'Ex'= 3)  
Ames$PoolQC = as.integer(revalue(Ames$PoolQC, Rate))  
table(Ames$PoolQC) %>% sum()  



# Data Cleansing: "Misc. Feature" column ----------------------------------  
Ames$MiscFeature[is.na(Ames$MiscFeature)] = "None"  
testing = aov(Ames.train$SalePrice~Ames.train$MiscFeature)
TukeyHSD(testing)
  # Response: After running post-hoc ad analysis, Misc Features do not contain significance between the 
  # differences in means of values in the feature
Misc = c('None'=0, 'Othr'=1, 'Shed'=2, 'Gar2'=3, 'TenC'=4)  
ggplot(Ames.train, aes(x = reorder(MiscFeature, SalePrice), y = SalePrice))+  
  geom_bar(stat = 'summary', fill = 'blue')  
Ames$MiscFeature = as.factor(Ames$MiscFeature) 



# Data Cleansing: "Alley" column ------------------------------------------  
Ames$Alley[is.na(Ames$Alley)]="None"  
Ames.train %>% group_by(Alley) %>% summarise(mean = median(SalePrice), count = n()) %>% arrange(mean) #Ordinarity exists
Alley = c('None'=0, 'Grvl'=1, 'Pave'=2) 
testing = aov(Ames.train$SalePrice~as.factor(Ames.train$Alley))
TukeyHSD(testing)
Ames$Alley = as.integer(revalue(Ames$Alley, Alley))  
Ames$NoAlley = as.integer((Ames$Alley == "None") * 1)
table(Ames$Alley) %>% sum()  



# Data Cleansing: "Fence" column ------------------------------------------  
Ames$Fence[is.na(Ames$Fence)] = "None"  
Ames.train %>% group_by(Fence) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean) #No ordinality  
testing = aov(Ames.train$SalePrice~Ames.train$Fence)
TukeyHSD(testing)
Ames$Fence = as.factor(Ames$Fence)
table(Ames$Fence) %>% sum()  



# Data Cleansing: "Fireplace.Qu" column --------------------------------------  
Ames$FireplaceQu[is.na(Ames$FireplaceQu)] = "None"   
Ames.train %>% group_by(FireplaceQu) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There is ordinality  
Quality = c('Ex'=5, 'Gd'=4, 'TA'=3, 'Fa'=2, 'Po'=1, 'None'=0)  
Ames$FireplaceQu = as.integer(revalue(Ames$FireplaceQu, Quality))  
table(Ames$FireplaceQu) %>% sum()  



# Data Cleansing: "Mason Veneer Area"  ------------------------------  
Ames$MasVnrArea[is.na(Ames$MasVnrArea)] = 0  
table(Ames$MasVnrArea) %>% sum()  
Ames$MasVnrArea = as.integer(Ames$MasVnrArea)  
# "Mason Veneer Type" column ----------------------------------------------  
Ames$MasVnrType[is.na(Ames$MasVnrArea)] = "None"  
Ames$MasVnrType[is.na(Ames$MasVnrType)] = "None"  
Ames[is.na(Ames$MasVnrArea)|is.na(Ames$MasVnrType), c('MasVnrArea', 'MasVnrType')]  
Ames.train %>% group_by(MasVnrType) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean) #There is ordinality for Veneer type  
testing = aov(Ames.train$SalePrice~Ames.train$MasVnrType)
TukeyHSD(testing)
Mason = c('None'=0, 'BrkCmn'=0, 'CBlock'=1, 'BrkFace'=2, 'Stone'=3)  
Ames$MasVnrType = as.integer(revalue(Ames$MasVnrType, Mason))  
table(Ames$MasVnrType) %>% sum()  



# Data Cleansing: "Garage" columns ---------------------------------------------------  
#Replace all 159 missing GarageYrBlt values with the year garage was built  
Ames$GarageYrBlt[is.na(Ames$GarageYrBlt)] = Ames$YearBuilt[is.na(Ames$GarageYrBlt)]  
Ames$GarageYrBlt[2593] = 2007  
Ames$GarageYrBlt[2512] = 1923  
Ames.train %>% group_by(GarageYrBlt) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There is ordinarity  
Ames$GarageYrBlt = as.integer(Ames$GarageYrBlt)  
table(Ames$GarageYrBlt) %>% sum()  
#Found a missing value in garage area column  
Ames[is.na(Ames$GarageType)|is.na(Ames$GarageCond)|is.na(Ames$GarageQual)|is.na(Ames$GarageFinish), c('GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')]  
Ames$GarageArea[2577] = 0  
Ames$GarageArea = as.integer(Ames$GarageArea)  
table(Ames$GarageArea) %>% sum()  
#Found a missing value in garage car column  
Ames$GarageCars[2577] = 0  
Ames.train %>% group_by(GarageCars) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #No ordinarity  
table(Ames$GarageCars) %>% sum()  
#Found a missing value in garage finish column  
Ames$GarageFinish[is.na(Ames$GarageFinish)] = "None"   
Ames$GarageFinish[2127] = names(sort(-table(Ames$GarageFinish)))[[1]] 
Ames$GarageFinish[2577] = names(sort(-table(Ames$GarageFinish)))[[1]] 
Ames.train %>% group_by(GarageFinish) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There is ordinarity  
testing = aov(Ames.train$SalePrice~Ames.train$GarageFinish)
TukeyHSD(testing)
Garage = c('Fin'=3, 'RFn'=2, 'Unf'=1, 'None'=0)  
Ames$GarageFinish = as.integer(revalue(Ames$GarageFinish, Garage))  
table(Ames$GarageFinish) %>% sum()  
#Found a missing value in garage type column  
Ames$GarageType[is.na(Ames$GarageType)] = "None"  
Ames.train %>% group_by(GarageType) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #No ordinarity  
Ames$GarageType = as.factor(Ames$GarageType)
table(Ames$GarageType) %>% sum()  
#Found a missing value in garage quality column  
Ames$GarageQual[is.na(Ames$GarageQual)] = "None"  
Ames$GarageQual[2127] = "TA"
Ames$GarageQual[2577] = "TA"
Ames.train %>% group_by(GarageQual) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There's ordinarity  
Ames$GarageQual = as.integer(revalue(Ames$GarageQual, Quality))  
table(Ames$GarageQual) %>% sum()  
#Found a missing value in garage condition column  
Ames$GarageCond[is.na(Ames$GarageCond)] = "None"  
Ames$GarageCond[2127] = "TA" 
Ames$GarageCond[2577] = "TA"
Ames.train %>% group_by(GarageCond) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There's ordinarity  
Ames$GarageCond = as.integer(revalue(Ames$GarageCond, Quality))  
table(Ames$GarageCond) %>% sum()  



# Data Cleansing: "Lot Frontage" column -----------------------------------  
for (i in 1:nrow(Ames)){  
  if(is.na(Ames$LotFrontage[i])){  
    Ames$LotFrontage[i] = as.integer(median(Ames$LotFrontage[Ames$Neighborhood==Ames$Neighborhood[i]], na.rm = TRUE))  
  }  
}  


# Data Cleansing: "Basement" columns ---------------------------------------  
#Found a missing value in basement quality column  
Ames[is.na(Ames$BsmtCond)|is.na(Ames$BsmtQual)|is.na(Ames$BsmtFinType1)|is.na(Ames$BsmtFinType2)|  
is.na(Ames$BsmtFullBath)|is.na(Ames$BsmtFinSF1)|is.na(Ames$TotalBsmtSF)|is.na(Ames$BsmtExposure), c('BsmtCond', 'BsmtQual', 'BsmtFinType1', 'BsmtFinType2', 'BsmtFullBath', 'BsmtFinSF1', 'TotalBsmtSF', 'BsmtExposure')]  
Ames$BsmtQual[is.na(Ames$BsmtQual)] = "None"  
Ames$BsmtQual[2218] = names(sort(-table(Ames$BsmtQual)))[[1]]  
Ames$BsmtQual[2219] = names(sort(-table(Ames$BsmtQual)))[[1]]  
Ames.train %>% group_by(BsmtQual) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There is ordinality  
Quality = c('Ex'=5, 'Gd'=4, 'TA' =3, 'Fa' =2, 'None'=0, 'Po'=1)  
Ames$BsmtQual = as.integer(revalue(Ames$BsmtQual, Quality))  
table(Ames$BsmtQual) %>% sum()  
#Found a missing value in basement condition column  
Ames$BsmtCond[is.na(Ames$BsmtCon)] = "None"  
Ames$BsmtCond[2041] = names(sort(-table(Ames$BsmtCond)))[[1]]  
Ames$BsmtCond[2186] = names(sort(-table(Ames$BsmtCond)))[[1]]  
Ames$BsmtCond[2525] = names(sort(-table(Ames$BsmtCond)))[[1]]  
Ames.train %>% group_by(BsmtCond) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There is ordinality  
Ames$BsmtCond = as.integer(revalue(Ames$BsmtCond, Quality))  
table(Ames$BsmtCond) %>% sum()  
#Found a missing value in basement exposure column  
Ames$BsmtExposure[is.na(Ames$BsmtExposure)] = "None"  
Ames$BsmtExposure[c(949, 1488, 2349)] = names(sort(-table(Ames$BsmtExposure)))[[1]]  
Ames.train %>% group_by(BsmtExposure) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There is ordinality  
Exposure = c('Gd'=4, 'Av'=3, 'Mn'=2, 'No'=1, 'None'=0)  
Ames$BsmtExposure = as.integer(revalue(Ames$BsmtExposure, Exposure))  
table(Ames$BsmtExposure) %>% sum()  
#Found a missing value in basement finish type 1 column  
Ames$BsmtFinType1[is.na(Ames$BsmtFinType1)] = "None"  
Ames.train %>% group_by(BsmtFinType1) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean) #No ordinality  
Ames$BsmtFinType1 = as.factor(Ames$BsmtFinType1)
table(Ames$BsmtFinType1) %>% sum()  
#Found a missing value in basement finish type 2 column  
Ames$BsmtFinType2[is.na(Ames$BsmtFinType2)] = "None"  
Ames$BsmtFinType2[333] = names(sort(-table(Ames$BsmtFinType2)))[[1]]  
Ames.train %>% group_by(BsmtFinType2) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean) #No ordinality  
Ames$BsmtFinType2 = as.factor(Ames$BsmtFinType2)
table(Ames$BsmtFinType2) %>% sum()  
#Found a missing value in basement finish SF 1 column  
Ames$BsmtFinSF1[is.na(Ames$BsmtFinSF1)] = 0  
Ames[is.na(Ames$BsmtFinSF1)|is.na(Ames$BsmtFinSF2)|is.na(Ames$TotalBsmtSF)|is.na(Ames$BsmtUnfSF)|is.na(Ames$BsmtCond), c('BsmtFinSF1', 'BsmtFinSF2', 'TotalBsmtSF', 'BsmtUnfSF', 'BsmtCond')]  
Ames$BsmtFinSF1 = as.integer(Ames$BsmtFinSF1)  
table(Ames$BsmtFinSF1) %>% sum()  
#Found a missing value in basement finish SF 2 column  
Ames$BsmtFinSF2[is.na(Ames$BsmtFinSF2)] = 0  
Ames$BsmtFinSF2 = as.integer(Ames$BsmtFinSF2)  
table(Ames$BsmtFinSF2) %>% sum()  
#Found a missing value in basement unfinished SF 1 column  
Ames$BsmtUnfSF[is.na(Ames$BsmtUnfSF)] = 0  
Ames$BsmtUnfSF = as.integer(Ames$BsmtUnfSF)  
table(Ames$BsmtUnfSF) %>% sum()  
#Found a missing value in total basement SF column  
Ames$TotalBsmtSF[is.na(Ames$TotalBsmtSF)] = 0  
Ames$TotalBsmtSF = as.integer(Ames$TotalBsmtSF)  
table(Ames$TotalBsmtSF) %>% sum()  
#Found a missing value in basement full bath column  
Ames$BsmtFullBath[is.na(Ames$BsmtFullBath)] = 0  
Ames$BsmtFullBath = as.integer(Ames$BsmtFullBath)  
table(Ames$BsmtFullBath) %>% sum()  
#Found a missing value in basement half bath column  
Ames$BsmtHalfBath[is.na(Ames$BsmtHalfBath)] = 0  
Ames$BsmtHalfBath = as.integer(Ames$BsmtHalfBath)  
table(Ames$BsmtHalfBath) %>% sum()  


# Convert categorical variables to ordinal variables, if necessary -----------  
#Convert external quality column to ordinal  
Ames.train %>% group_by(ExterQual) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There's ordinarity  
Exter = c('Ex'=5, 'Gd'=4, 'TA'=3, 'Fa'= 2)  
Ames$ExterQual = as.integer(revalue(Ames$ExterQual, Exter))  
table(Ames$ExterQual) %>% sum()  
#Convert external conditional column to ordinal  
Ames$ExterCond = as.integer(revalue(Ames$ExterCond, Quality))  
table(Ames$ExterCond) %>% sum()  
#Convert heating QC column to ordinal  
Ames.train %>% group_by(HeatingQC) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean)  
Ames$HeatingQC = as.integer(revalue(Ames$HeatingQC, Quality))  
table(Ames$HeatingQC)  %>% sum()
#Convert central air conditioning to ordinal  
Ames.train %>% group_by(CentralAir) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There's ordinarity  
Condition = c('Y'=2, 'N'=0)  
testing = aov(Ames.train$SalePrice~Ames.train$CentralAir)
TukeyHSD(testing)
Ames$CentralAir = as.integer(revalue(Ames$CentralAir, Condition))  
table(Ames$CentralAir) %>% sum()  
#Convert kitchen quality to ordinal  
Ames[is.na(Ames$KitchenAbvGr)|is.na(Ames$KitchenQual), c('KitchenAbvGr', 'KitchenQual')]  
Ames$KitchenQual[1556] = names(sort(-table(Ames$KitchenQual[Ames$KitchenAbvGr %in% 1])))[[1]]  
Ames.train %>% group_by(KitchenQual) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #There's ordinarity  
Ames$KitchenQual = as.integer(revalue(Ames$KitchenQual, Quality))  
table(Ames$KitchenQual) %>% sum()  
#Convert paved drive to ordinal  
Ames.train %>% group_by(PavedDrive) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean) #There's ordinarity  
Paved = c('Y'=2, 'P'=1, 'N'=0)  
testing = aov(Ames.train$SalePrice~Ames.train$PavedDrive)
TukeyHSD(testing)
Ames$IsPaved = as.integer((Ames$PavedDrive == 'Y') *1)
Ames$PavedDrive = as.integer(revalue(Ames$PavedDrive, Paved))  
table(Ames$PavedDrive) %>% sum()  



# Data Cleansing: Sale Type -----------------------------------------------  
Ames[is.na(Ames$SaleType)|is.na(Ames$SaleCondition), c('SaleType', 'SaleCondition')]  
Ames$SaleType[2490] = names(sort(-table(Ames$SaleType[Ames$SaleCondition %in% 'Normal'])))[[1]]  
Ames.train %>% group_by(SaleType) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  
Ames$SaleType = as.factor(Ames$SaleType)
table(Ames$SaleType) %>% sum()  



# Datal Cleansing: Exterior 1st and 2nd -----------------------------------  
Ames[is.na(Ames$YearRemodAdd)|is.na(Ames$RoofMatl)|is.na(Ames$Exterior1st)|is.na(Ames$Exterior2nd)|is.na(Ames$YearBuilt), 
     c('YearRemodAdd', 'RoofMatl', 'Exterior1st', "Exterior2nd", 'YearBuilt')]  
Ames$Exterior1st[2152] = 'MetalSd' 
Ames$Exterior2nd[2152] = names(sort(-table(Ames$Exterior2nd[Ames$YearBuilt %in% 1940])))[[1]]  
Ames.train %>% group_by(Exterior2nd) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  
Ames$Exterior1st = as.factor(Ames$Exterior1st)
Ames$Exterior2nd = as.factor(Ames$Exterior2nd)



# Data Cleansing: MSZoning ------------------------------------------------  
Ames[is.na(Ames$MSZoning)|is.na(Ames$MSSubClass), c('MSZoning', 'MSSubClass')]  
Ames$MSZoning[c(2217, 2905)] = names(sort(-table(Ames$MSZoning[Ames$MSSubClass %in% 20])))[[1]]   
Ames$MSZoning[1916] = names(sort(-table(Ames$MSZoning[Ames$MSSubClass %in% 30])))[[1]]  
Ames$MSZoning[2251] = names(sort(-table(Ames$MSZoning[Ames$MSSubClass %in% 70])))[[1]]  
Ames.train %>% group_by(MSZoning) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean) #No ordinarity  
testing = aov(Ames.train$SalePrice~as.factor(Ames.train$MSZoning))
TukeyHSD(testing)
Zoning = c('FV' = 4, 'RL'=3, 'RH'=2, 'RM'=2, 'C (all)'=0) #post ad-hoc validates that RH and RM are no different
Ames$MSZoning = as.integer(revalue(Ames$MSZoning, Zoning)) 
table(Ames$MSZoning) %>% sum()



# Data Cleansing: Utilities -----------------------------------------------  
table(Ames$Utilities)  
col.drop = c('Utilities')  
Ames = Ames[, !names(Ames) %in% col.drop]  



# Data Cleansing: Functional ----------------------------------------------  
table(Ames$Functional) %>% sum()  
Ames$Functional[is.na(Ames$Functional)] = 'Typ'  
Ames.train %>% group_by(Functional) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) #No ordinarity  
Ames$Functional = as.factor(Ames$Functional)



# Data Cleansing: Electrical ----------------------------------------------  
table(Ames$Electrical) %>% sum()  
Ames[is.na(Ames$Electrical)|is.na(Ames$Heating), c('Electrical', 'Heating')]  
Ames$Electrical[1380] = names(sort(-table(Ames$Electrical[Ames$Heating %in% 'GasA'])))[[1]]  
Ames.train %>% group_by(Electrical) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  #No ordinarity
Ames$Electrical = as.factor(Ames$Electrical)



# Observe and factorize the remaining character variables -----------------------------------  
#Street  
Ames.train %>% group_by(Street) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean)
testing = aov(Ames.train$SalePrice~Ames.train$Street)
TukeyHSD(testing)
street = c('Grvl'=0, 'Pave'=1) 
Ames$Street = as.integer(revalue(Ames$Street, street)) 
table(Ames$Street)
#Land Slope  
table(Ames$LandSlope)  
Ames.train %>% group_by(LandSlope) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) 
testing = aov(Ames.train$SalePrice~Ames.train$LandSlope)
TukeyHSD(testing)
Ames$LandSlope = as.factor(Ames$LandSlope)
#LandContour  
table(Ames$LandContour)  
Ames.train %>% group_by(LandContour) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  
testing = aov(Ames.train$SalePrice~Ames.train$LandContour)
TukeyHSD(testing)
Ames$LandContour = as.factor(Ames$LandContour)
#Lot Config  
table(Ames$LotConfig) %>% sum() 
Ames.train %>% group_by(LotConfig) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  
testing = aov(Ames.train$SalePrice~Ames.train$LotConfig)
TukeyHSD(testing)
Ames$LotConfig = as.factor(Ames$LotConfig)
#Lot Shape  
table(Ames$LotShape)  
Ames.train %>% group_by(LotShape) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  
testing = aov(Ames.train$SalePrice~Ames.train$LotShape)
TukeyHSD(testing)
Ames$LotShape = as.factor(Ames$LotShape)
#Condition 1
table(Ames$Condition1)  
Ames$Condition1 = as.factor(Ames$Condition1)
#Condition 2  
Ames.train %>% group_by(Condition2) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean)  
Ames$Condition2 = as.factor(Ames$Condition2)
#Building Type  
table(Ames$BldgType)
testing = aov(Ames.train$SalePrice~Ames.train$BldgType)
TukeyHSD(testing)
Ames.train %>% group_by(BldgType) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)
Building = c('2fmCon'=1, 'Duplex'=1, 'Twnhs'=1, '1Fam'=2, 'TwnhsE'=2)
Ames$BldgType = as.integer(revalue(Ames$BldgType, Building))
#House Style  
Ames.train %>% group_by(HouseStyle) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean) 
testing = aov(Ames.train$SalePrice~Ames.train$HouseStyle)
TukeyHSD(testing)
Ames$HouseStyle = as.factor(Ames$HouseStyle)
#Roof Style  
Ames.train %>% group_by(RoofStyle) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  
testing = aov(Ames.train$SalePrice~Ames.train$RoofStyle)
TukeyHSD(testing)
Ames$RoofStyle = as.factor(Ames$RoofStyle)
#Roof Matl  
Ames.train %>% group_by(RoofMatl) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  
testing = aov(Ames.train$SalePrice~Ames.train$RoofMatl)
TukeyHSD(testing)
Ames$RoofMatl = as.factor(Ames$RoofMatl)
#Heating  
Ames.train %>% group_by(Heating) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  
testing = aov(Ames.train$SalePrice~Ames.train$Heating)
TukeyHSD(testing)
Ames$Heating = as.factor(Ames$Heating) 
#Foundation  
Ames.train %>% group_by(Foundation) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  
testing = aov(Ames.train$SalePrice~Ames.train$Foundation)
TukeyHSD(testing)
Ames$Foundation = as.factor(Ames$Foundation)
#Sale Type  
Ames.train %>% group_by(SaleType) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)
testing = aov(Ames.train$SalePrice~Ames.train$SaleType)
TukeyHSD(testing)
Ames$SaleType = as.factor(Ames$SaleType)
#Sale Condition  
Ames.train %>% group_by(SaleCondition) %>% summarise(mean = mean(SalePrice), count=n()) %>% arrange(mean) 
testing = aov(Ames.train$SalePrice~Ames.train$SaleCondition)
TukeyHSD(testing)
Ames$IsPartial = as.integer((Ames$SaleCondition == 'Partial') * 1)
Ames$SaleCondition = as.factor(Ames$SaleCondition) 



# FEATURING ENGINEERING ------------------------------------------------------------  
#Total square feet of a house-------------------------------------------------------  
Ames = Ames %>% mutate(TotalArea = TotalBsmtSF+GrLivArea)  
Ames$TotalArea = as.integer(Ames$TotalArea)  
#Total porch space of a house--------------------------------------------------------  
Ames = Ames %>% mutate(TotalOutdoorSF = OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch)  
Ames$TotalOutdoorSF = as.integer(Ames$TotalOutdoorSF)  
#Total number of bathrooms ---------------------------------------------------------  
Ames = Ames %>% mutate(TotalBaths = BsmtFullBath + BsmtHalfBath*(0.5) + FullBath + HalfBath*(0.5))  
Ames$TotalBaths = as.integer(Ames$TotalBaths)  
#Age of a house at the time of sale--------------------------------------------------  
Ames = Ames %>% mutate(HouseAge = Ames$YrSold - YearRemodAdd)  
Ames$HouseAge[c(524, 2296, 2550)] = 0 #The house was already bought before it was built so set this equal to 0.  
Ames$HouseAge = as.integer(Ames$HouseAge)  
ggplot(Ames.train, aes(x=HouseAge, y = SalePrice))+
  geom_bar(stat = 'summary', fun.y = median, fill = 'sky blue')+  
  scale_y_continuous(breaks = seq(50000, 1000000, by = 50000))+  
  geom_smooth(method=lm, color = 'black')  
#Which one is a new house-----------------------------------------  
Ames$HouseNew = ifelse(Ames$YrSold == Ames$YearBuilt, 1, 0)
Ames$HouseNew = as.integer(Ames$HouseNew)
ggplot(Ames.train, aes(x=HouseNew, y = SalePrice, fill = HouseNew))+  
  geom_bar(stat = 'summary', fun.y = median) 
#Which one is remodelled -------------------------------------------------
Ames$Remodelled = ifelse(Ames$YearRemodAdd != Ames$YearBuilt, 0, 1)
Ames$Remodelled = as.integer(Ames$Remodelled)
ggplot(Ames.train, aes(x = Remodelled, y = SalePrice, fill = Remodelled))+
  geom_bar(stat = 'identity', fun.y = median)+
  theme(legend.title = element_text(color = 'blue', size = 10, face = 'bold'), 
        legend.text = element_text(color = 'black', size = 10))
#Observe neighborhood and then bin neighborhood ------------------------------------------  
ggplot(Ames.train, aes(x = reorder(Neighborhood, SalePrice, FUN = median), y = SalePrice))+  
  geom_bar(stat = 'summary', fun.y = median, fill = 'sky blue')+  
  theme(axis.text.x = element_text(angle = 90, hjust =1))+  
  scale_y_continuous(breaks = seq(50000, 400000, by = 50000))+  
  labs(x = 'Neighborhood', y = 'Sale Price')+  
  geom_hline(yintercept = median(Ames.train$SalePrice), linetype = 'dashed', color = 'red')+  
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)  
nbrh.map <- c('MeadowV' = 0, 'IDOTRR' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1,   
              'BrkSide' = 1, 'Sawyer' =2, 'Blueste' = 2, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,  
              'SawyerW' = 3, 'Gilbert' = 3, 'NWAmes' = 3, 'Blmngtn' = 3, 'CollgCr' = 3, 'ClearCr' = 3,   
              'Crawfor' = 3, 'Veenker' = 4, 'Somerst' = 4, 'Timber' = 4, 'StoneBr' = 5, 'NoRidge' = 5,   
              'NridgHt' = 5)  
Ames['NeighborhoodBin'] = as.numeric(nbrh.map[Ames$Neighborhood])
Ames$Neighborhood = as.factor(Ames$Neighborhood)
#Observe year built and then bin year built column -----------------------------------  
timedata = Ames.train[, c(17,90)] %>% arrange(YearBuilt) %>% group_by(as.factor(YearBuilt)) %>% 
  summarise(YearBuilt = mean(SalePrice))
timedata = ts(timedata)
Price_Time = timedata[, -1]
ggtsdisplay(Price_Time, xlab = "Time", ylab = "Sale Price", main = "Price v Time")

timedata2 = Ames.train[, c(19,88)] %>% arrange(YearRemodAdd) %>% group_by(as.factor(YearRemodAdd)) %>% summarize(YearRemodAdd = mean(SalePrice))
timedata2 = ts(timedata2)
Price_Time2 = timedata2[, -1]
ggtsdisplay(Price_Time2, xlab = "Time", ylab = "Sale Price", main = "Price v YearRemodAdd")
  # Response: Although there's an increasing trend, there's some cyclical trend in sight.   
    # It displays cyclical trend representing economic upturns and downturns across the timeline, therefore it is
    # hard to capture this effect through encoding. We will not encode, however, bin peaked saleprice in
    # some of the years. Clearly, YearRemodAdd has higher predictive power due to less fluctuations across the years. The 
    # cyclical trends are more visible as seen by time series. 
# Observe GrLivArea and the Outliers ----------------------------------------------------------------
b=ggplot(Ames.train, aes(x=GrLivArea, y = SalePrice))+
  geom_point()+
  scale_y_continuous(breaks = seq(50000, 800000, by = 50000))+
  geom_smooth(method = lm, color = 'blue')+
  geom_smooth(method = loess, color = 'red')+
  geom_text_repel(aes(label = ifelse(Ames.train$GrLivArea > 4000, rownames(Ames.train), "")))
a=ggplot(Ames.train, aes(x=TotalArea, y = SalePrice))+
  geom_point()+
  scale_y_continuous(breaks = seq(50000, 800000, by = 50000))+
  geom_smooth(method = lm, color = 'blue')+
  geom_smooth(method = loess, color = 'red')+
  geom_text_repel(aes(label = ifelse(Ames.train$TotalArea > 7000, rownames(Ames.train), "")))
grid.arrange(a,b)
Ames = Ames[-c(524,1299), ]
train = train[-c(524,1299),] #Updating for outliers
Ames.train = Ames[1:1458, ]  
Ames.train$SalePrice = train$SalePrice
  #Response: Since both 692 and 1183 are in the trend, we will keep them. Then, outliers, in this case, seem to be
  # 524 and 1299 and we will eliminate them from the dataset. 


# Find all highly correlated numeric variables ------------------------------------------  
numericVar = which(sapply(Ames.train, is.numeric))  
Ames_numVar = Ames.train[, numericVar]
Cor_numVar = cor(Ames_numVar, use = "pairwise.complete.obs")  
Cor_sorted = as.matrix(sort(Cor_numVar[, 'SalePrice']), decreasing = TRUE)  
Cor_high = names(which(apply(Cor_sorted, 1, function(x) abs(x)>0.5)))  
Cor_numVar = Cor_numVar[Cor_high, Cor_high]  
corrplot.mixed(Cor_numVar, tl.col = "black", number.cex = .8, tl.pos = "lt")  




# To reduce multicollinearity find highly correlated variables --------------------
Cor_numVar[is.na(Cor_numVar)] = 0
findCorrelation(Cor_numVar, cutoff = 0.75)
drop.cols1 = colnames(Cor_numVar[, c(10,22,28,38,44,45,42,56,51)])# w/ feature engineering 
drop.cols2 = colnames(Cor_numVar[, c(41,10,22,28,38,44,45,56,51)])# w/o feature engineering
Ames = Ames[, !names(Ames) %in% drop.cols1]
Ames = Ames[, !names(Ames) %in% drop.cols2]
  # Remove attributes with absolute value of the correlation of 0.75 or higher
  # Remove TotRmsAbvGr since it's highly correlated with GrLivArea. But the one that is correlated higher with a response
  #   variable will be kept. 

cor(Ames.train$GarageArea, Ames.train$SalePrice)

# Rank Features by Importance -------------------------------------------------------  
set.seed(5)  
rf.ames = randomForest(x=Ames.train[, -88], y=Ames.train$SalePrice, ntree = 100, importance =TRUE)  
imp_rf = importance(rf.ames)  
imp_df = data.frame(variables = row.names(imp_rf), MSE = imp_rf[, 1])  
imp_df = imp_df[order(imp_df$MSE, decreasing = TRUE), ]  
ggplot(imp_df[1:20, ], aes(x = reorder(variables, MSE), y = MSE, fill = MSE))+  
  geom_bar(stat = 'identity')+  
  coord_flip()+  
  ylab("MSE % Increase")  
#The feature importance is computed by averaging the gain of each feature for all split and all trees in the model.  
#Take a look at the 10 most important features used in our model.   
#It contains two measures of variable importance: Gini gain and permutation importance. Since the permutation importance
# is affected by collinearity, it is necessary to handle collinearity prior to running random forest for extracting
# important variables. 



# Feature Selection based on the Importance (Recurvsive Feature Elimination) -------------------------------
Imp_var = Ames.train[, names(Ames.train) %in% row.names(imp_rf)]
control = rfeControl(functions = rfFuncs, method = "cv", number = 10)
results = rfe(Imp_var[, 1:90], Ames.train$SalePrice, sizes = c(1:90), rfeControl = control)
plot(results, type = c('g', 'o'))
  #Response: Will not perform this due to being computationally inefficient.



# Lastly, factorize numeric variables before preparing modeling ---------------------
#These are classes so should be converted to factor  
table(Ames$MSSubClass)  
Ames.train %>% group_by(MSSubClass) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean) 
Ames$MSSubClass=as.factor(Ames$MSSubClass)
#There's seasonality in months  
table(Ames$MoSold)
Ames.train %>% group_by(MoSold) %>% summarise(mean = median(SalePrice), count=n()) %>% arrange(mean)  
Ames$MoSold = as.factor(Ames$MoSold)
#Year Sold
Ames$YrSold = as.factor(Ames$YrSold)
