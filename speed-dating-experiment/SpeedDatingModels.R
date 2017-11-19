Speed_Dating_Data = read.csv("Speed Dating Data.csv")
data = Speed_Dating_Data

#Create test set: setting aside 7 waves for test set
set.seed(6)
test = sample(21,7)
Dtest = subset(data,wave %in% test)

#Create training set: 14 waves in training set
Dtrain = subset(data, !(wave %in% test))




###################################################################
##Logistic Regression 

#creating the data frame
logisticdata = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age_o, Dtrain$race_o, Dtrain$samerace,Dtrain$dec)
logisticdata = na.omit(logisticdata)
colnames(logisticdata) = c("iid", "gender", "age_o", "race_o", "same_race","dec")

logisticdatatesting = data.frame(Dtest$iid, Dtest$gender, Dtest$age_o, Dtest$race_o, Dtest$samerace,Dtest$dec)
logisticdatatesting = na.omit(logisticdatatesting)
colnames(logisticdatatesting) = c("iid", "gender", "age_o", "race_o", "same_race","dec")

################
#Aside: decision tree
library(rpart)
fit <- rpart(as.factor(dec)~age_o + gender+ as.factor(race_o) + as.factor(same_race),
             method="class", data=logisticdata)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits





#############


#fitting the full model
fullmodel = glm(as.factor(dec)~age_o + gender+ as.factor(race_o) + as.factor(same_race) ,family="binomial",data = logisticdata)
summary(fullmodel)

#visualizing that women are more choosy
females = subset(logisticdata,logisticdata$gender ==0)
female_dec = females$dec
hist(female_dec)
males = subset(logisticdata,logisticdata$gender ==1)
male_dec = males$dec
hist(males$dec)

#cross validation
library(leaps)
regfit.full = regsubsets(as.factor(dec)~age_o + gender+ as.factor(race_o) + as.factor(same_race),data = logisticdata,nvmax=4)
summary(regfit.full)


install.packages("caret")
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)


full_model <- train(as.factor(dec) ~  age_o + gender+ as.factor(race_o) + as.factor(same_race),data = logisticdata, family="binomial", trControl = ctrl)
View(full_model$pred)


pred = predict(mod_fit, newdata=logisticdatatesting)
pred








##################################
Dtrain = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$field_cd, Dtrain$attr_o,Dtrain$sinc_o,Dtrain$intel_o,Dtrain$fun_o,Dtrain$amb_o, Dtrain$attr3_1, Dtrain$sinc3_1, Dtrain$intel3_1, Dtrain$fun3_1, Dtrain$amb3_1, Dtrain$like_o)
Dtrain = na.omit(Dtrain)
colnames(Dtrain) = c("iid", "gender", "age", "race", "field", "attr_o","sinc_o","intel_o","fun_o","amb_o", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1", "like_o")


Dtrain = aggregate(Dtrain[, 1:length(Dtrain)], list(Dtrain$iid), mean)
#now like_o is basically popularity

Dtest = data.frame(Dtest$iid, Dtest$gender, Dtest$age, Dtest$race, Dtest$field_cd, Dtest$attr_o,Dtest$sinc_o,Dtest$intel_o,Dtest$fun_o,Dtest$amb_o, Dtest$attr3_1, Dtest$sinc3_1, Dtest$intel3_1, Dtest$fun3_1, Dtest$amb3_1, Dtest$like_o)
Dtest = na.omit(Dtest)
colnames(Dtest) = c("iid", "gender", "age", "race", "field", "attr_o","sinc_o","intel_o","fun_o","amb_o", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1", "like_o")

for (i in 1:nrow(Dtrain)){
  x = c(Dtrain[i,]$attr_o,Dtrain[i,]$sinc_o,Dtrain[i,]$intel_o,Dtrain[i,]$fun_o,Dtrain[i,]$amb_o)
  y = c(Dtrain[i,]$attr3_1,Dtrain[i,]$sinc3_1,Dtrain[i,]$intel3_1,Dtrain[i,]$fun3_1,Dtrain[i,]$amb3_1)
  Dtrain$self_awar_score[i] = mean(x-y)
}

##Predicting popularity with following features: age, gender, race, self-awareness, field
#need to drop field == 12 from training set because no one in test set is in that field
Dtrain = subset(Dtrain, Dtrain$field != 12)

model = lm(Dtrain$like_o~Dtrain$age + Dtrain$gender + Dtrain$age*Dtrain$gender + as.factor(Dtrain$race) + Dtrain$gender*as.factor(Dtrain$race) + as.factor(Dtrain$field))
summary(model)

library(glmnet)

model = lm(Dtrain$like_o~Dtrain$age + Dtrain$gender + Dtrain$age*Dtrain$gender + as.factor(Dtrain$race) + Dtrain$gender*as.factor(Dtrain$race) + as.factor(Dtrain$field))
coef(model)

X_train <- model.matrix(Dtrain$like_o ~ Dtrain$age + Dtrain$gender + Dtrain$age*Dtrain$gender + as.factor(Dtrain$race) + Dtrain$gender*as.factor(Dtrain$race) + as.factor(Dtrain$field))[,-1]
y_train <- Dtrain$like_o
m = lm(y~x)
coef(model)

# The Lasso
lambdas = 10^seq(10,-2,length = 100)
cv.out=cv.glmnet(X_train,y_train,alpha=1, lambda = lambdas)
bestlam=cv.out$lambda.min
lasso.mod=glmnet(X_train,y_train,alpha=1,lambda=bestlam)
lasso.coef=coef(lasso.mod)[,1]

#Calculating the test MSE
Dtest = aggregate(Dtest[, 1:length(Dtest)], list(Dtest$iid), mean)

X_te <- model.matrix(Dtest$like_o ~ Dtest$age + Dtest$gender + Dtest$age*Dtest$gender + as.factor(Dtest$race) + Dtest$gender*as.factor(Dtest$race) + as.factor(Dtest$field))[,-1]
y_te = Dtest$like_o
pred.lasso = predict(lasso.mod, s = bestlam, newx = X_te)
mean((pred.lasso - y_te)**2)

#Later adding back in attractiveness and the other predictors

#Decision tree for classification of Yes or No
Dtrain = subset(data, !(wave %in% test))

DTrain = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$same_race, Dtrain$order, Dtrain$age_diff, Dtrain$int_corr, Dtrain$match)
colnames(Dtrain) = c("iid", "gender", "age", "race", "same_race", "order","age_diff","int_corr", "match")








