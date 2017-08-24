## SYS 6018 HW1 ##
## Teresa Sun (js6sm) ##

library(readr)  
library(dplyr)

# load in data
titanic.train <- read_csv('train.csv')
test <- read_csv('test.csv')

# change the class into factor for some variables
titanic.train$Survived <- as.factor(titanic.train$Survived)
titanic.train$Pclass <- as.factor(titanic.train$Pclass)
titanic.train$Sex <- as.factor(titanic.train$Sex)
titanic.train$Embarked <- as.factor(titanic.train$Embarked)

test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)

test$Age[is.na(test$Age)] <- mean(test$Age, na.rm = TRUE)

# split the dataset into train and valid
sub <- sample(1:nrow(titanic.train),size = nrow(titanic.train)/2)
train <- titanic.train[sub,]     
valid <- titanic.train[-sub,]


## Logistics regression
p1 <- glm(Survived ~ Pclass+Sex +Age+ SibSp+Parch+Fare+Embarked, data = train, family = 'binomial')
summary(p1)
# Pclass, Sex, Age, SibSp are significant

# test on training set
probs<-as.vector(predict(p1, type="response"))
preds <- rep(0,445)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
table(preds,train$Survived)
(171+66)/445
# 0.5325843

# test on validation set
probs<-as.vector(predict(p1,newdata=valid, type="response"))
preds <- rep(0,446)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
table(preds,valid$Survived)
(243+97)/446
# 0.7623318

## Model 1: Sex
glm1 <- glm(Survived ~ Sex, data = train, family = 'binomial')
summary(glm1)
# AIC 466.77

# test on training set
probs<-as.vector(predict(glm1, type="response"))
preds <- rep(0,445)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
table(preds,train$Survived)
(234+113)/445
# 0.7797753

# test on validation set
probs<-as.vector(predict(glm1,newdata=valid, type="response"))
preds <- rep(0,446)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
table(preds,valid$Survived)
(234+120)/446
# 0.793722

## Model 2: Pclass, Sex, Age, SibSp CHOSEN
glm2 <- glm(Survived ~ Pclass+Sex +Age+ SibSp, data = train, family = 'binomial')
summary(glm2)
# AIC 336.68

# test on training set
probs<-as.vector(predict(glm2, type="response"))
preds <- rep(0,445)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
table(preds,train$Survived)
(160+68)/445
# 0.5123596

# test on validation set
probs<-as.vector(predict(glm2,newdata=valid, type="response"))
preds <- rep(0,446)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
table(preds,valid$Survived)
(240+104)/446
# 0.7713004

## model 3: Pclass, Sex, Age
glm3 <- glm(Survived ~ Pclass +Sex +Age, data = train, family = 'binomial')
summary(glm3)
# AIC 341.55

# test on training set
probs<-as.vector(predict(glm3, type="response"))
preds <- rep(0,445)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
table(preds,train$Survived)
(158+72)/445
# 0.5168539

# test on validation set
probs<-as.vector(predict(glm3,newdata=valid, type="response"))
preds <- rep(0,446)  # Initialize prediction vector
preds[probs>0.5] <- 1 # p>0.5 -> 1
preds
table(preds,valid$Survived)
(233+105)/446
# 0.7578475


# Make Predictions using the Test Set
p.hats <- predict.glm(glm2, newdata = test, type = "response")
survival = ifelse(p.hats > 0.5, 1, 0)

# Creating CSV for Kaggle Submission
sub <- cbind(test$PassengerId, survival)
colnames(sub) <- c("PassengerId", "Survived")
write.csv(sub, file = "js6sm-titanic-prediction.csv", row.names = FALSE)



