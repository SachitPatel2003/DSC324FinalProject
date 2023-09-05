nba1 <- read.csv("2022-2023 NBA Player Stats - Regular.csv", sep=";")
head(nba1)
library(tidyverse)
library(corrplot)
library(psych)
library(ggplot2)
library(glmnet)
library(leaps)
library(GGally)
library(car)

#let's view the structure of the dataset
str(nba1)
nba1

summary(nba1)
colSums(is.na(nba1))
num_teams <- nba1 %>% select(Tm) %>% n_distinct() #finding the number of teams
num_teams   #There are 31 teams

#Checking for duplicate rows
duplicates_df <- nba1[duplicate_values, ]
print(duplicates_df) #140 obs

#These players seem to have played in diffrent teams in the season, 
#so we will keep the stats. we will however remove the rows called total.

#drop duplicate rows TOT
nba3 <- nba1[nba1$Tm != "TOT", ]

#Creating dummy variables for Position to ease correlation plots
nba_new <- nba3 %>% 
  mutate(Position_dummy = ifelse(Pos == "PG", 1, 
                                 ifelse(Pos == "SG", 2, 
                                        ifelse(Pos == "SF", 3,
                                               ifelse(Pos== "PF", 4, 5)))))

head(nba_new)

# Compute the correlation matrix
cor_matrix <- cor(nba_new[,-c(1,2,3,5)])#excluding categorical with original position variable
cor_matrix

#creatring plots to examine the data
pairs.panels(nba_new[,c(-1, -2, -3, -5)])

corrplot(cor_matrix,)
#From the pairs.panels plot
#age is slightly skewed, FG is slightly left skewed and very highly correlated with PTS
#FGA is sligthl left skewed, X3P, X3PA, X2P, X2PA and X2P% are left skewed.
#eFG is fairly normal 
#FT is left skewed, FTA is right skewed, CRB is left skewed, DRB is left skewed, TRB is right skewed
#AST is left skewed, STL is left skewed, BL is left skewed, TOR is left skewed, PT is normal PTS is left skewed.

#Lets perform log transforms on them.


nba_new$GS = log(nba_new$GS+1)
nba_new$FG = log(nba_new$FG+1)
nba_new$FGA = log(nba_new$FGA+1)
nba_new$X3P = log(nba_new$X3P+1)
nba_new$X3PA = log(nba_new$X3PA+1)
nba_new$X3P. = log(nba_new$X3P.+1)
nba_new$X2P = log(nba_new$X2P+1)
nba_new$X2PA = log(nba_new$X2PA+1)
nba_new$X2P. = log(nba_new$X2P.+1)
nba_new$eFG. = log(nba_new$eFG.+1)
nba_new$FT = log(nba_new$FT+1)
nba_new$FTA = log(nba_new$FTA+1)
nba_new$FT. = log(nba_new$FT.+1)
nba_new$ORB = log(nba_new$ORB+1)
nba_new$DRB = log(nba_new$DRB+1)
nba_new$TRB = log(nba_new$TRB+1)
nba_new$AST = log(nba_new$AST+1)
nba_new$STL = log(nba_new$STL+1)
nba_new$BLK = log(nba_new$BLK+1)
nba_new$TOV = log(nba_new$TOV+1)
nba_new$PTS = log(nba_new$PTS+1)
hist(nba_new$PTS)

pairs.panels(nba_new[,c(-1, -2, -3, -5)])

#lets check the new distributions

P1 <- ggplot(nba_new, aes(x= GS)) + geom_histogram()
P2 <- ggplot(nba_new, aes(x= FG)) + geom_histogram()
P3 <- ggplot(nba_new, aes(x= FGA)) + geom_histogram()
P4 <- ggplot(nba_new, aes(x= X3P)) + geom_histogram()
P5 <- ggplot(nba_new, aes(x= X3P.)) + geom_histogram()
P6 <- ggplot(nba_new, aes(x= X2P)) + geom_histogram()
P7 <- ggplot(nba_new, aes(x= eFG.)) + geom_histogram()
P8 <- ggplot(nba_new, aes(x= FT)) + geom_histogram()
P9 <- ggplot(nba_new, aes(x= FTA)) + geom_histogram()
P10 <- ggplot(nba_new, aes(x= ORB)) + geom_histogram()
P11 <- ggplot(nba_new, aes(x= FT.)) + geom_histogram()
P12 <- ggplot(nba_new, aes(x= DRB)) + geom_histogram()
P13 <- ggplot(nba_new, aes(x= TRB)) + geom_histogram()
P14 <- ggplot(nba_new, aes(x= AST)) + geom_histogram()
P15 <- ggplot(nba_new, aes(x= STL)) + geom_histogram()
P16 <- ggplot(nba_new, aes(x= BLK)) + geom_histogram()
P17 <- ggplot(nba_new, aes(x= TOV)) + geom_histogram()
P18 <- ggplot(nba_new, aes(x= PTS)) + geom_histogram()

library(gridExtra)

grid.arrange(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14,P15,P16,P17,P18, nrow = 4)

cor_matrix2 <- cor(nba_new[,-c(1,2,3,5)]) #doing new correlation matrix and plot
cor_matrix2

corrplot(cor_matrix2)

head(nba_new)

class(nba_new)


nba_new1 <- nba_new[, -c(1, 2, 3, 5)] #changed from 1, 2, 5, to 1, 2, 3 ,5 
nba_new1
nrow(nba_new1)
#Aliasing Error
fitAlias = lm(PTS~., data=nba_new1)
vif(fitAlias)
#EXTRA STEP: divide set into individual and team stats.
#Pull out team stats
playerStats <- nba_new[c(4, 6:8, 12:13, 15:16, 19:20, 22:23, 25:31)]
teamStats = nba_new[-c(1,2,3,5,9.18,24)]
head(playerStats)
summary(playerStats)
head(teamStats)
summary(teamStats)

#Using playerStats for analysis
set.seed(708)
nrow(playerStats)
s = sample(609, 539)
dsTrain = playerStats[s, ]
dsTest = playerStats[-s, ]

fit1 = lm(PTS ~., data=playerStats)
summary(fit1)
vif(fit1)
#Overfitting & Multicollinearity Present
#Multicollinearity Check

#theory behind multicollinearity elimination:
#remove "partial variables" and model around "complete variables"
#ex: remove 2p/3p per game + attempts per game and use percentage (removing 2 collinear variables)
dsTrain = dsTrain[, -c(4, 5, 7, 10, 11, 16)]
fit1 = lm(PTS ~., data=dsTrain)
summary(fit1)
vif(fit1)
#all that's left is minutes played per game

#remove the same variables from test 
dsTest = dsTest[, -c(4, 5, 7, 10, 11, 16)]
#Checking datasets
dsTrain
dsTest
#they have the same variables. 
#x & y splits 
#playerStats FULL MODEL
yTrain = as.matrix(playerStats[,18])
xTrain = as.matrix(playerStats[,-18])
##
yTrain = as.matrix(dsTrain[,18])
xTrain = as.matrix(dsTrain[,-18])
yTest = as.matrix(dsTest[,18])
xTest = as.matrix(dsTest[,-18])
#Splits with limited predictors
#train
yTrain = as.matrix(dsTrain[, 12])
yTrain #PTS variable
xTrain = as.matrix(dsTrain[, -12])
xTrain
#test
yTest = as.matrix(dsTest[, 12])
yTest
xTest = as.matrix(dsTest[, -12])
xTest



CompleteCases <- complete.cases(xTrain, yTrain)
CompleteCases
#Let me make an OLS model attempt
fit2Ols = lm(PTS~., data=dsTest)
summary(fit2Ols)
rmse_ols_train = sqrt(mean(fit2Ols$residuals^2))
rmse_ols_train
#there still seems to be some inflation.
#glmnet call, look at previous lectures before continuing
set.seed(918)
lRange = seq(0,5,.1)
fitRange = glmnet(xTrain, yTrain, data=dsTrain, lambda=lRange)
#Experimental
#Remove 2 point and 3 point 
playerStats = playerStats[,-c(5,7)]
xTrain = xTrain[,-c(5,7)]
xTest = xTest[,-c(5,7)]

fitRange = cv.glmnet(xTrain, yTrain, data=playerStats, alpha=1, nfolds=3)
plot(fitRange)
summary(fitRange)
coef(fitRange, s="lambda.1se")
fitRange$lambda.1se

glmnet(xTrain, yTrain, data=playerStats, lambda=lRange)
fit1se = glmnet(xTrain, yTrain, data=dsTrain, lambda ="lambda.1se")
####
summary(fitRange)
plot(fitRange)
fitLasso = cv.glmnet(xTrain, yTrain, relax=T, data=dsTrain)
print(fitLasso)
summary(fitLasso)
plot(fitLasso)
coef(fitLasso, s="lambda.min", gamma=0)
coef(fitLasso, s="lambda.1se", gamma=0)
coef(fitLasso, s="lambda.1se", gamma=1)
fitLasso$lambda.1se
#how do we get the dev%?
pLassoTrain = predict(fitLasso, xTrain, s="lambda.1se")
rmse_lasso_train = sqrt(mean((pLassoTrain - yTrain)^2))
rmse_lasso_train  # 0.18 with set seed

#glmnet with lambda
glmnet(xTrain, yTrain, data=dsTrain, relax=T, lambda=0.04653347)
fitLasso2 = glmnet(xTrain, yTrain, data=dsTrain, relax=T, lambda = 0.04653347)
summary(fitLasso2)
coef(fitLasso2, s="lambda.1se")
#RMSE
pLassoTrain2 = predict(fitLasso2, xTrain, s="lambda.1se")
rmse_lasso_train2 = sqrt(mean((pLassoTrain2 - yTrain)^2))
rmse_lasso_train2  #0.18

#Victor's RMSE code
#Checking
#predicting train set at lambda = 1se and RMSE of train set
#Used experimental 
lassopredtr = predict(fitRange, xTrain, s="lambda.1se") 
rmselassopredtr = sqrt(mean((lassopredtr - yTrain)^2))
rmselassopredtr

#predicting test set at lambda = 1se and RMSE of test set
lassopred = predict(fitRange, xTest, s="lambda.1se")
rmselassortest = sqrt(mean((lassopred - yTest)^2))
rmselassortest
#Ratio of RMSE of Train to test in Lasso cv model
rmselassortest / rmselassopredtr


#Anirudh's All Subset Test
#All subset
library(leaps)
#Splitting data
set.seed(8274)
sample <- sample(c(TRUE, FALSE), nrow(playerStats), replace=TRUE, prob=c(0.7,0.3))
nbatrain  <- playerStats[sample, ]
nbanewtrain = as.data.frame(nbatrain)
nbatest   <- playerStats[-sample, ]


#Model Building 
#All subset
library(leaps)
sub = regsubsets(PTS~., data = nbatrain, really.big = T, nvmax = length(playerStats))

test_predictor_matrix <- model.matrix(PTS ~ ., data = nbatest)[, -1]
summary(sub, test_data = test_predictor_matrix)

plot(subsets, scale="adjr2")
plot(subsets, sacle="bic")
plot(subsets, scale="cp")

#OLS
fit3Ols = lm(PTS~X3PA+X2PA+FT+DRB, data=dsTrain)
summary(fit3Ols)

#Alaa's Residual Analysis Code
#Residuals vs. Fitted
plot(fit3Ols, which=1)
#Normality
plot(fit3Ols, which=2)
#Residuals vs. Cook's Distance
plot(fit3Ols, which=4)
#remove specific observations, get a list of cook's distance.


#Interpret the coefficents of the relaxed lasso
