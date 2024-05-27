#Rosengren and Gaynor - Weightlifting Project Corresponding R Code
#Import data
library(haven)
library(NSM3)
library(tidyverse)
library(mosaic)
library(leaps)
library(HH)
library(MASS)
library(glmnet)
library(ridge)
library(Rfit)
library(ggplot2)
library(corrplot)
install.packages("markovchain")
library(markovchain)

weight <- read_csv("~/Downloads/Weightlifting Data - Sheet2.csv")
corrweight <- read_csv("~/Downloads/Weightlifting Data - Sheet4.csv")
#view(weight)


### Data by competition level
liftdata <- weight

#EDA
boxplot(CJ1a~CJ1s.,data=liftdata)
boxplot(CJ2a~CJ2s.,data=liftdata)
boxplot(CJ3a~CJ3s.,data=liftdata)

favstats(CJ1a~CJ1s.,data=liftdata)[c(1,2,4,6,7,8,9)]
favstats(CJ2a~CJ2s.,data=liftdata)[c(1,2,4,6,7,8,9)]
favstats(CJ3a~CJ3s.,data=liftdata)[c(1,2,4,6,7,8,9)]

### Data by rep level

#EDA
boxplot(CJa~CJs,data=liftdata)
favstats(CJa~CJs,data=liftdata)

#Next, we took a look at logistic regression models to predict the proportion of 
#successes enjoyed by Wilkes using his age, bodyweight, and previous attempt results. 
#The best model for this was our cj3_model, which made use of the weights and binary outcome 
#(success/failure) of each of the first two attempts.

#Logistic regression (not included in final presentation)
CJallmodel <- glm(CJs~CJa+Age+BW,family=binomial, data = liftdata2)
summary(CJallmodel)

#Logistic Regression by CJ attempt
cj1_model <- glm(CJ1s ~ CJ1a+Age+BW, data = weight)
summary(cj1_model)

cj2_model <- glm(CJ2s ~ CJ1a+CJ1s+CJ2a+Age+BW, data = weight)
summary(cj2_model)

#Best model by CJ attempt
cj3_model <- glm(CJ3s ~CJ2a+CJ2s+CJ1a+CJ1s+CJ3a+Age+BW, data = weight)
summary(cj3_model)

#Because our logistic regression indicated a significant bodyweight coefficient, 
#we wanted to see if this trend was industry-wide. For the 109+ KG Ultra heavyweight class, 
#participants enjoy a seeming competitive advantage from having an increased bodyweight. 
#This is reflected by a negative linear correlation between rank (where 1 is the best) and bodyweight 
#and a positive linear correlation between total weight lifted (sum of snatch and clean and jerk weights) 
#and bodyweight.

plot(corrweight$BW, corrweight$TW)  # Plot the scatter plot
abline(lm(corrweight$TW ~ corrweight$BW), col = "red") 

plot(corrweight$BW, corrweight$Rank)  # Plot the scatter plot
abline(lm(corrweight$Rank ~ corrweight$BW), col = "red") 

plot(weight$BW, weight$TW)  # Plot the scatter plot
abline(lm(weight$TW ~ weight$BW), col = "red") 

adjweight <- subset(weight, BW >= 109)
plot(adjweight$BW, adjweight$PFT)  # Plot the scatter plot
abline(lm(adjweight$PFT ~ adjweight$BW), col = "red") 


#Next, a decision tree model to illustrate the proportion of Wilkes' successes based 
#on the weights he chose for each successive attempt. This decision tree model was then used 
#to fuel our Markov Chain based simulations.

#Decision tree model
tree_modelLift2 <- rpart(CJs~CJa+Age+BW,data = liftdata2)
rpart.plot(tree_modelLift2)
plotcp(tree_modelLift2)

#Using Markov Chain theory, we constructed the following four strategies that Wilkes can use 
#in his future weightlifting competitions. The first, labeled his current strategy, is a safe approach 
#for competition. This likely would be used when Wilkes puts up his expected numbers on the snatch, and 
#finds himself in a comfortable position where he doesn't need to make up any ground with his clean and 
#jerk numbers.

#Current Strategy
liftStates = c("zero", "209", "217","224")
byRow = TRUE
liftMatrix = matrix(data = c(0.05, 0.95,0, 0,
                             0,0.62,0.38, 0,
                             0,0,0.8,0.2,
                             0,0,0,1), byrow = byRow, nrow = 4,
                    
                    dimnames = list(liftStates, liftStates))

mcCaineW = new("markovchain", states = liftStates, byrow = byRow,
               transitionMatrix = liftMatrix, name = "Caine")

initialState = c(1,0,0,0)
after3Attempts = initialState * (mcCaineW^3)
print(after3Attempts)

#The next, his aggressive strategy, is a more apt strategy when his snatch doesn't go according to plan. 
#In this case, he may find himself in a position where he has to put up some real numbers on the clean and 
#jerk in order to place, or even win, a competition. 

#Aggressive Strategy
liftStates = c("zero", "217", "224","230")
byRow = TRUE
liftMatrix = matrix(data = c(0.62, 0.38,0, 0,
                             0,0.8,0.2, 0,
                             0,0,0.8,0.2,
                             0,0,0,1), byrow = byRow, nrow = 4,
                    
                    dimnames = list(liftStates, liftStates))

mcCaineW = new("markovchain", states = liftStates, byrow = byRow,
               transitionMatrix = liftMatrix, name = "Caine")

initialState = c(1,0,0,0)
after3Attempts = initialState * (mcCaineW^3)
print(after3Attempts)

#Wilkes' hourglass strategy is a testament to his age. As the oldest lifter on Team USA in the 2020 
#Tokyo olympics, Wilkes is at a disadvantage compared to his peers when it comes to his natural athleticism 
#and power output. With the hourglass strategy, Wilkes will use his wisdom to get on the board with his first 
#attempt which is a near-lock. Then, he will jump to a weight that younger, less experienced professionals may 
#not be able to reach, allowing him to take full advantage of his experience and technical prowess.

#Hourglass Strategy
liftStates = c("zero", "209", "224","230")
byRow = TRUE
liftMatrix = matrix(data = c(0.05, 0.95,0, 0,
                             0,0.8,0.2, 0,
                             0,0,0.8,0.2,
                             0,0,0,1), byrow = byRow, nrow = 4,
                    
                    dimnames = list(liftStates, liftStates))

mcCaineW = new("markovchain", states = liftStates, byrow = byRow,
               transitionMatrix = liftMatrix, name = "Caine")

initialState = c(1,0,0,0)
after3Attempts = initialState * (mcCaineW^3)
print(after3Attempts)

#The last strategy is perfect for the Paris Olympics this summer. If he competes, Wilkes will almost 
#assuredly find himself going up against Lasha Talakhadze. In order to beat this juggernaut, he must go where 
#he has never gone before. It's all or nothing. With this strategy, Wilkes will sacrifice the safety of an easier 
#first attempt in order to go for gold. If successful, his last attempt would represent a personal record, and 
#while it may not be enough to trump Lasha in competition, it would almost surely be enough to get him a spot on 
#the Olympic podium.

#Gold Medal Strategy
liftStates = c("zero", "224", "230","230+")
byRow = TRUE
liftMatrix = matrix(data = c(0.8, 0.2,0, 0,
                             0,0.8,0.2, 0,
                             0,0,0.8,0.2,
                             0,0,0,1), byrow = byRow, nrow = 4,
                    
                    dimnames = list(liftStates, liftStates))

mcCaineW = new("markovchain", states = liftStates, byrow = byRow,
               transitionMatrix = liftMatrix, name = "Caine")

initialState = c(1,0,0,0)
after3Attempts = initialState * (mcCaineW^3)
print(after3Attempts)


