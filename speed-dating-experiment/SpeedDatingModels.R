#####################################################################################
## Splitting Data
#####################################################################################

Speed_Dating_Data = read.csv("Speed Dating Data.csv")
data = Speed_Dating_Data

#Create test set: setting aside 7 waves for test set
set.seed(6)
test = sample(21,7)
Dtest = subset(data,wave %in% test)

#Create training set: 14 waves in training set
Dtrain = subset(data, !(wave %in% test))


###################################################################
##Decision Tree on Your Decision 
###################################################################

library(rpart)
fit <- rpart(as.factor(dec)~gender + as.factor(race_o) + as.factor(same_race) + age_o + as.factor(field_cd),
             method="class", data=logisticdata)
#fit = rpart(as.factor(dec)~as.factor(gender) +  as.factor(same_race) + as.factor(race) + as.factor(race_o) + age + age_o + int_corr + as.factor(goal) + date + go_out + exphappy, method = "class", data = logisticdata)
fit = rpart(as.factor(dec)~as.factor(goal), method = "class", data = logisticdata)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
print(fit)
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE, 
     main="Classification Tree for Decision")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


#install.packages("rattle")
library(rpart.plot)
rpart.plot(fit,cex=1.2)

############################ Prune the tree
train_waves = c(1,2,3,4,5,8,9,10,11,12,17,18,20,21)

cps = 10^seq(10,-20,length = 100)
bestMean = 0
bestCP = NA
d = dim(logisticdata[1])[1]

for (c in 1:length(cps)){
  print(c)
  m=0
  for (i in train_waves){
    cv.index = which(logisticdata$wave == i)
    Test = logisticdata[cv.index,]
    Train = logisticdata[-cv.index,]
    #fit = rpart(as.factor(dec)~as.factor(gender) +  as.factor(same_race) + as.factor(race) + as.factor(race_o) + age + age_o + int_corr + as.factor(goal) + date + go_out + exphappy, method = "class", data = Train)
    fit = rpart(as.factor(dec)~as.factor(goal), method = "class", data = Train)
    pfit<- prune(fit, cp=cps[c])
    pred <- predict(pfit, Test)
    pred = pred < 0.5
    pred = pred[,1]
    #print(mean(pred == Test$dec))
    m = m + (dim(Test)[1]/d)*mean(pred == Test$dec)
    print(dim(Test)[1])
  }
  print(m)
  if (m > bestMean){
    bestMean = m
    bestCP = cps[c]
  }
}

pfit<- prune(fit, cp=bestCP)
print(pfit)

printcp(pfit) # display the results 
plotcp(pfit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Decision")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

test = data.frame(Dtest$goal, Dtest$dec)
test = na.omit(test)
colnames(test) = c("goal","dec")

pred <- predict(pfit, test)

pred = pred < 0.5
pred = pred[,1]

mean(pred == test$dec)

#############



###################################################################
##LASSO Predicting Popularity 
###################################################################
Dtrain = subset(data, !(wave %in% test))
Dtrain = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$field_cd, Dtrain$attr_o,Dtrain$sinc_o,Dtrain$intel_o,Dtrain$fun_o,Dtrain$amb_o, Dtrain$attr3_1, Dtrain$sinc3_1, Dtrain$intel3_1, Dtrain$fun3_1, Dtrain$amb3_1, Dtrain$goal, Dtrain$date, Dtrain$go_out, Dtrain$exphappy, Dtrain$like_o)
colnames(Dtrain) = c("iid", "gender", "age", "race", "field", "attr_o","sinc_o","intel_o","fun_o","amb_o", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1", "goal", "date", "go_out", "exphappy", "like_o")
Dtrain = na.omit(Dtrain)

Dtrain = aggregate(Dtrain[, 1:length(Dtrain)], list(Dtrain$iid), mean)
#now like_o is basically popularity

set.seed(6)
test = sample(21,7)
Dtest = subset(data,wave %in% test)

Dtest = data.frame(Dtest$iid, Dtest$gender, Dtest$age, Dtest$race, Dtest$field_cd, Dtest$attr_o,Dtest$sinc_o,Dtest$intel_o,Dtest$fun_o,Dtest$amb_o, Dtest$attr3_1, Dtest$sinc3_1, Dtest$intel3_1, Dtest$fun3_1, Dtest$amb3_1, Dtest$goal, Dtest$date, Dtest$go_out, Dtest$exphappy,Dtest$like_o)
colnames(Dtest) = c("iid", "gender", "age", "race", "field", "attr_o","sinc_o","intel_o","fun_o","amb_o", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1", "goal", "date", "go_out", "exphappy", "like_o")
Dtest = na.omit(Dtest)

for (i in 1:nrow(Dtrain)){
  x = c(Dtrain[i,]$attr_o,Dtrain[i,]$sinc_o,Dtrain[i,]$intel_o,Dtrain[i,]$fun_o,Dtrain[i,]$amb_o)
  y = c(Dtrain[i,]$attr3_1,Dtrain[i,]$sinc3_1,Dtrain[i,]$intel3_1,Dtrain[i,]$fun3_1,Dtrain[i,]$amb3_1)
  Dtrain$self_awar_score[i] = mean(x-y)
}

##Predicting popularity with following features: age, gender, race, interaction, field
#need to drop field == 12 from training set because no one in test set is in that field
Dtrain = subset(Dtrain, Dtrain$field != 12)
Dtrain = subset(Dtrain, Dtrain$field != 16)
#Dtrain = data.frame(Dtrain$like_o, Dtrain$age, Dtrain$gender, Dtrain$race, Dtrain$field, Dtrain$goal, Dtrain$date, Dtrain$go_out, Dtrain$exphappy)
#Dtrain = na.omit(Dtrain)

model = lm(Dtrain$like_o~Dtrain$age + Dtrain$gender + Dtrain$age*Dtrain$gender + as.factor(Dtrain$race) + Dtrain$gender*as.factor(Dtrain$race) + as.factor(Dtrain$field) + as.factor(Dtrain$goal) + Dtrain$date + Dtrain$go_out + as.numeric(Dtrain$exphappy))
summary(model)
coef(model)

library(glmnet)

X_train <- model.matrix(Dtrain$like_o ~ Dtrain$age + Dtrain$gender + Dtrain$age*Dtrain$gender + as.factor(Dtrain$race) + Dtrain$gender*as.factor(Dtrain$race) + as.factor(Dtrain$field) + as.factor(Dtrain$goal) + Dtrain$date + Dtrain$go_out + as.numeric(Dtrain$exphappy))[,-1]
y_train <- Dtrain$like_o
m = lm(y~x)
coef(model)

# The Lasso

#alphas = c(200, 576, 160, 648, 162, 200, 800, 162, 882, 392, 280, 72, 84, 924)/5542
lambdas = 10^seq(10,-2,length = 100)
cv.out=cv.glmnet(X_train,y_train,alpha=1, lambda = lambdas)

bestlam=cv.out$lambda.min
lasso.mod=glmnet(X_train,y_train,alpha=1,lambda=bestlam)
lasso.coef=coef(lasso.mod)[,1]
plot(cv.out,lambdas)


#Calculating the test MSE
Dtest = aggregate(Dtest[, 1:length(Dtest)], list(Dtest$iid), mean)

X_te <- model.matrix(Dtest$like_o ~ Dtest$age + Dtest$gender + Dtest$age*Dtest$gender + as.factor(Dtest$race) + Dtest$gender*as.factor(Dtest$race) + as.factor(Dtest$field) + as.factor(Dtest$goal) + Dtest$date + Dtest$go_out + as.numeric(Dtest$exphappy))[,-1]
y_te = Dtest$like_o
pred.lasso = predict(lasso.mod, s = bestlam, newx = X_te)
mean((pred.lasso - y_te)**2)

z = mean(Dtrain$like_o, na.rm=TRUE)
mean((rep(z, length(y_te)) - y_te)**2)

###################################################################
##Logistic Regression on Your Decision
###################################################################

#creating the data frame
logisticdata = data.frame(Dtrain$dec, Dtrain$wave, Dtrain$gender, Dtrain$race, Dtrain$race_o, Dtrain$samerace, Dtrain$age, Dtrain$age_o,Dtrain$int_corr,Dtrain$goal,Dtrain$date,Dtrain$go_out, Dtrain$exphappy,Dtrain$field_cd)
logisticdata = na.omit(logisticdata)
colnames(logisticdata) = c("dec","wave", "gender", "race", "race_o", "same_race","age","age_o","int_corr","goal","date","go_out","exphappy","field_cd")

logisticdata_test = data.frame(Dtest$dec, Dtest$wave, Dtest$gender, Dtest$race, Dtest$race_o, Dtest$samerace, Dtest$age, Dtest$age_o,Dtest$int_corr,Dtest$goal,Dtest$date,Dtest$go_out, Dtest$exphappy,Dtest$field_cd)
logisticdata_test = na.omit(logisticdata_test)
colnames(logisticdata_test) = c("dec","wave", "gender", "race", "race_o", "same_race","age","age_o","int_corr","goal","date","go_out","exphappy","field_cd")

#visualization
library(plyr)
count(Speed_Dating_Data$dec)
counts = table(Speed_Dating_Data$dec,Speed_Dating_Data$race_o)
rownames(counts) = c("Decision = No","Decision = Yes")
barplot(counts, main="Partner's Race", xaxt = "n", col=c("darkblue","red"),
        legend = rownames(counts))
prob = subset(Speed_Dating_Data, Speed_Dating_Data$race==4)
mean(prob$dec,na.rm=TRUE)
prob = subset(Speed_Dating_Data, Speed_Dating_Data$race==2)
mean(prob$dec,na.rm=TRUE)
prob = subset(Speed_Dating_Data, Speed_Dating_Data$race_o==4)
mean(prob$dec,na.rm=TRUE)
prob = subset(Speed_Dating_Data, Speed_Dating_Data$race_o==2)
mean(prob$dec,na.rm=TRUE)

#visualizing gender
females = subset(logisticdata,logisticdata$gender ==0)
female_dec = females$dec
hist(female_dec)
males = subset(logisticdata,logisticdata$gender ==1)
male_dec = males$dec
hist(males$dec)

dec_0 = subset(Dtrain, Dtrain$dec ==0)
hist(dec_0$like)
dec_0_female = subset(dec_0, dec_0$gender == 0)
hist(dec_0_female$like)
dec_0_male = subset(dec_0, dec_0$gender ==1)
hist(dec_0_male$like)

dec_1 = subset(Dtrain, Dtrain$dec ==1)
hist(dec_1$like)
dec_1_female = subset(dec_1, dec_1$gender ==0)
hist(dec_1_female$like)
dec_1_male = subset(dec_1, dec_1$gender ==1)
hist(dec_1_male$like)


#feature engineering
logisticdata$asian_o = ifelse(logisticdata$race_o ==4, 1, 0)
logisticdata$europeanlatino = ifelse(logisticdata$race ==2 |logisticdata$race ==3 , 1, 0)
logisticdata$white = ifelse(logisticdata$race ==2 |logisticdata$race ==3 , 1, 0)
logisticdata$goal4 = ifelse(logisticdata$goal ==4, 1, 0)

logisticdata_test$asian_o = ifelse(logisticdata_test$race_o ==4, 1, 0)
logisticdata_test$europeanlatino = ifelse(logisticdata_test$race ==2 |logisticdata_test$race ==3 , 1, 0)
logisticdata_test$white = ifelse(logisticdata_test$race ==2 |logisticdata_test$race ==3 , 1, 0)
logisticdata_test$goal4 = ifelse(logisticdata_test$goal ==4, 1, 0)

fullmodel = glm(as.factor(dec)~as.factor(gender)*as.factor(asian_o) + as.factor(asian_o)*as.factor(europeanlatino) +  as.factor(same_race) + age + age_o + age*age_o + int_corr + as.factor(goal4) + date + go_out + exphappy,family="binomial",data = logisticdata)
summary(fullmodel)
model = glm(as.factor(dec)~ as.factor(field_cd),family="binomial",data = logisticdata)
summary(model)

#cross validation
acc <- NULL

train_waves = c(1,2,3,4,5,8,9,10,11,12,17,18,20,21)
for (i in train_waves){
  cv.index = which(logisticdata$wave == i)
  test = logisticdata[cv.index,]
  train = logisticdata[-cv.index,]
  model = glm(as.factor(dec)~as.factor(as.factor(field_cd)),  family="binomial",data = train)
  results_prob <- predict(model,test,type='response')
  results <- ifelse(results_prob > 0.5,1,0)
  answers <- test$dec
  misClasificError <- mean(answers != results)
  acc = c(acc, 1-misClasificError)
}
avgmisClasificError = mean(misClasificError)
avgmisClasificError

#test classification error
results_prob <- predict(model,logisticdata_test,type='response')
results <- ifelse(results_prob > 0.5,1,0)
answers <- logisticdata_test$dec
misClasificError <- mean(answers != results)
acc = c(acc, 1-misClasificError)
avgmisClasificError = mean(misClasificError)
avgmisClasificError

num_predic = c(14,13,12,11,10,9,8,7,6,5,4,3,2,1)
CV_error = c(0.4621212,0.465368,0.4599567,0.4588745,0.461039,0.4599567,0.4588745,0.4556277,0.4632035,0.465368,0.4761905,0.461039,0.478355,0.5411255)
plot(CV_error,type = "o",xlab = "Number of Predictors",main="CV Errors w/ Backwards Selection")

