#MODEL PREPARATION : REQUIREMENTS
library(glmnet)
library(randomForest)
library(caret) #Remove zero-variance predictors for overfitting issues
library(ggrepel)
library(psych) #Skewness and kurtosis
library(factoextra)
library(FactoMineR)
library(gridExtra)
library(DMwR)
library(xgboost)
library(caretEnsemble)



# Split numerical and categorical (ordinal and nominal) variables ------------------------------
numVar = which(sapply(Ames, is.numeric))
Ames_catVar = Ames[, -numVar]
Ames_numVar = Ames[, numVar]
str(Ames_numVar)


# Transform all (true) numerical data that are skewed: (transform non-linear relationships into linear relationships) -----------------------------------------
  #Skewness
true_numVar = c('LotFrontage', 'LotArea', 'YearRemodAdd','MasVnrArea','BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF','X1stFlrSF', 'LowQualFinSF',
                'X2ndFlrSF', 'GarageYrBlt','WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch','X3SsnPorch','ScreenPorch', 'MiscVal',
                'TotalArea','TotalOutdoorSF')
true_numVar = Ames_numVar[, true_numVar]
for(i in 1:ncol(true_numVar)){
  if(abs(skew(true_numVar [, i]))>0.7){
    true_numVar[, i] = log(true_numVar[, i]+1)
  }
}



# standardize all normalized numerical data (normality assumptions should be met for z scores) -------------------------------
scale_numtrain = Ames_numVar[1:1458, colnames(true_numVar)]
scale_numtest = Ames_numVar[1459:nrow(Ames_numVar), colnames(true_numVar)]
scale.train = preProcess(scale_numtrain, method = c("center", "scale"))
scale.train = predict(scale.train, scale_numtrain)
scale.test = preProcess(scale_numtest, method = c("center", "scale"))
scale.test = predict(scale.test, scale_numtest)
  #Scaling is done separately to prevent "leaking" of information from the test set into the training set.

#Now, let's combine both scaled datasets for use in PCA. The dataset should be normalized (mean of zero and 
  # same standard deviation) to ensure that no variables are weighted unfairly. You won't get mean zero if you use
  # dummy coding.
scaled.df = rbind(scale.train, scale.test)



# Perform dimension reduction on (true) numerical data: PCA ----------------------------------------
#Part 1: Visualize PCA
pca.train = PCA(scale.train, scale.unit = FALSE, graph = FALSE)
summary(pca.train) #Dimensions 155 = 95% of information contained
dimdesc(pca.train, axes = 1:2)
fviz_eig(pca.train, addlabels = TRUE)
pca.train$eig %>% View()
pca.train$ind$coord %>% View()
fviz_contrib(pca.train, choice = "var", axes =1:5, top = 15)

#Part 2: Extract the principal components
pr.out = prcomp(scale.train, scale. = F) #dimensions 17 = 100% of information contained
summary(pr.out)
pr.data = pr.out$x
pr.train.data = data.frame(pr.data, SalePrice = train$SalePrice)
pr.train.data = pr.train.data[, 1:29]
pr.test.data = predict(pr.out, newdata = scale.test)
pr.test.data = data.frame(pr.test.data, SalePrice = final.test$SalePrice)
pr.test.data = pr.test.data[, 1:29]



# Combine scaled numeric and other numeric predictors ---------------------
scaled=colnames(scaled.df)
Ames_scaledtrain = Ames_numVar[1:1458, !names(Ames_numVar)%in%scaled]
Ames_scaledtest = Ames_numVar[1459:nrow(Ames_numVar), !names(Ames_numVar)%in%scaled]
scaled_numtrain = cbind(Ames_scaledtrain, scale.train)
scaled_numtest = cbind(Ames_scaledtest, scale.test)



# One-hot encode the rest of the categorical predictors -------------------
dfdummies = as.data.frame(model.matrix(~.-1, Ames_catVar))
dummies.train = dfdummies[1:1458, ]
dummies.test = dfdummies[1459:nrow(dfdummies), ]
  # Response: A reason that dummy variables aren't scaled or normalized is that it cannot be treated as a 
  # quantitative variables and the results would be meaningless.



# Combine both numerical and categorical predictors, then separate into training and testing -----------------------
final.train = cbind(dummies.train, scaled_numtrain)
final.train$SalePrice = train$SalePrice
final.test = cbind(dummies.test, scaled_numtest)
final.test$SalePrice = NA



# Test for normality and skewness of response variable ---------------------------------------------
log(final.train$SalePrice) %>% skew() # 0.0653 meets the -0.8 to 0.8 threshold
log(final.train$SalePrice) %>% kurtosi() #0.655 meets the -3.0 to 3.0 threshold
par(mfrow=c(2,1))
qqline(log(final.train$SalePrice))
hist(log(final.train$SalePrice), main = "Histogram of Sale Price", xlab = "Sale Price", col = "blue")
final.train$SalePrice = log(final.train$SalePrice) 



# OPTIONAL: Drop dummy variables with little or no observations -------------------------------------------
nzv.train = nearZeroVar(final.train, saveMetrics = TRUE)
nzv.test = nearZeroVar(final.test, saveMetrics = TRUE)
drop.cols1 = rownames(nzv.train)[nzv.train$zeroVar == TRUE]
drop.cols1 = "NoAlley"
final.train = final.train[, !names(final.train) %in% drop.cols1]
final.test = final.test[, !names(final.test) %in% drop.cols1]
  #Dropping columns with near zero variance would cause too much information to be lost.
fewcols = which(colSums(final.train[1:nrow(final.train), ])<10)
final.droptrain = final.train[, -fewcols]
final.droptest = final.test[, -fewcols]



# Make models: Lasso, Ridge, and Elastic -------------------------------------------------------------
#a: Set controls for Lasso
set.seed(102091)
control = trainControl(method = "repeatedcv", number = 10, repeats = 2)
lasso_grid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, by = 0.0005))
#a: Fit Lasso
lasso.fit = train(SalePrice~., data = final.train, method = 'glmnet', trControl = control,
                  tuneGrid = lasso_grid)
#a: Predict Lasso
pred.lasso = predict(lasso.fit, final.test)
prediction.lasso = data.frame(Id = seq(1461, 2919, 1), SalePrice = exp(pred.lasso))
practice = write.csv(prediction.lasso, file = 'prediction.lasso.csv', row.names = FALSE)


#b: Set controls for Ridge---------------------------------------------------------------------------
set.seed(102091)
ridge_grid = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.0005))
#b: Fit Ridge
ridge.fit = train(SalePrice~., data = final.train, method = 'glmnet', trControl = control,
                   tuneGrid = ridge_grid)
#b: Predict Ridge
pred.ridge = predict(ridge.fit, final.test)
prediction.ridge = data.frame(Id = seq(1461, 2919, 1), SalePrice = exp(pred.ridge))
pratice = write.csv(prediction.ridge, file = 'prediction.ridge.csv', row.names = FALSE)


#c: Set controls for Elastic Net-----------------------------------------------------------------
set.seed(102091)
elastic_grid = expand.grid(alpha = 0.5, lambda = seq(0.001, 0.1, by = 0.0005))
#c: Fit Elastic Net
elastic.fit = train(SalePrice~., data = final.train, method = 'glmnet', trControl = control,
                   tuneGrid = elastic_grid)
#c: Predict Elastic Net
pred.elastic = predict(elastic.fit, final.test)
prediction.elastic = data.frame(Id = seq(1461, 2919, 1), SalePrice = exp(pred.elastic))
pratice = write.csv(prediction.elastic, file = 'prediction.elastic.csv', row.names = FALSE)


#d: Set controls for XGBoost----------------------------------------------------------------------
set.seed(102091)
cv.control = trainControl(method = "repeatedcv", number = 10, repeats = 2, allowParallel = TRUE)
XGBoost_grid = expand.grid(gamma = 0, nrounds = 500, max_depth=seq(2,8,1), eta = c(0.1, 0.05, 0.01), colsample_bytree=1, 
                           subsample=1, min_child_weight=c(1,2,3,4,5))
xgb_grid_opt = expand.grid(objective = 'reg:linear', booster = 'gbtree', gamma = 0, nrounds = 500, max_depth=3, eta = 0.05, 
                           colsample_bytree=1, subsample=1, min_child_weight=4)
#d: Tune & Fit XGBoost
XGBoost.fit = train(SalePrice~., data = final.train, method = 'xgbTree', trControl = cv.control,
                    tuneGrid = XGBoost_grid, metric = "RMSE")
#d: Predict XGBoost
pred.xgb = predict(XGBoost.fit, final.test)
prediction.xgb = data.frame(Id = seq(1461, 2919, 1), SalePrice = exp(pred.xgb))
pratice = write.csv(prediction.xgb, file = 'prediction.XGBoost.csv', row.names = FALSE)
#d: Display feature importance matrix
features = xgb.importance(feature_names = colnames(final.train), model = XGBoost.fit)
xgb.ggplot.importance(importance_matrix = features[1:20], rel_to_first = TRUE)



# Weighted Average Ensemble -------------------------------------------------------
Gridlasso = expand.grid(.alpha = 1, .lambda = seq(0.001,0.1,by = 0.001))
Gridridge = expand.grid(.alpha = 0, .lambda = seq(0.001,0.1,by = 0.001))
Gridelastic = expand.grid(.alpha = 0.5, .lambda = seq(0.001,0.1,by = 0.001))
trcontrol = trainControl(method = "cv", number = 10, repeats = 2, savePredictions = 'final')

modelList = caretList(x=final.train, y=final.train$SalePrice, trControl=trcontrol, metric="RMSE", tuneList = list(
  xgbTree = caretModelSpec(method = "xgbTree", tuneGrid = XGBoost_grid, nthread = 8),
  glmnet = caretModelSpec(method = "glmnet", tuneGrid = Gridlasso),
  glmnet = caretModelSpec(method = "glmnet", tuneGrid = Gridridge),
  glmnet = caretModelSpec(method = "glmnet", tuneGrid = Gridelastic)
))
set.seed(102091)
#a: Fit Ensemble
Ensemble = caretEnsemble(modelList, metric = "RMSE", trControl = control)
#a: Predict Ensemble
pred.ensemble = predict(Ensemble, final.test)
prediction.ensemble = data.frame(Id = seq(1461, 2919, 1), SalePrice = exp(pred.ensemble))



# Simple Average of all models ----------------------------------------------------------
pred.s_average = (prediction.elastic$SalePrice+prediction.ridge$SalePrice+prediction.lasso$SalePrice)/3
predictions.s_average = data.frame(Id = seq(1461, 2919, 1), SalePrice = pred.s_average)
pratice = write.csv(predictions.s_average, file = 'prediction.s_average.csv', row.names = FALSE)



# Weighted Average of all models ------------------------------------------
pred.w_average = (prediction.elastic$SalePrice+prediction.ridge$SalePrice+prediction.lasso$SalePrice)/3
predictions.w_average = data.frame(Id = seq(1461, 2919, 1), SalePrice = pred.w_average)


# Stacking ----------------------------------------------------------------
elastic.fit$pred

