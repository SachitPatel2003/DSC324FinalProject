nba1 <- read.csv("NBA_Player_stats.csv", sep=";")
head(nba1)
library(tidyverse)
library(corrplot)
library(psych)
library(ggplot2)

#let's view the structure of the dataset
str(nba1)

summary(nba1)

colSums(is.na(nba1))

num_teams <- nba1 %>% select(Tm) %>% n_distinct() #finding the number of teams
num_teams   #There are 31 teams

#Checking for duplicate rows
duplicates_df <- nba1[duplicate_values, ]
print(duplicates_df) #140 obs

#These players seem to have played in diffrent teams in the season, so we will keep the stats. we will however remove the rows called total.

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
#fit an initial OLS regression m

class(nba_new)


nba_new1 <- nba_new[, -c(1, 2, 5)]

fit1 = lm(PTS ~., data=nba_new1)
summary(fit1)

# first off, the R-squared and Adj R-squared seems to be too high at over 99%. this indicates  overfitting and multicollinearity.

#The model seems to find G, GS,MP,FG,FGA, X3P, X3PA, X3P%, X2P, eFG%, FTA, FT% as significant. This is nothing new as the cor plots show this.

library(car)

vif(fit1)


