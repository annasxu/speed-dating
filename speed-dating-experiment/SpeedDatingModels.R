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
logisticdata = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age_o, Dtrain$race_o, Dtrain$samerace,Dtrain$dec, Dtrain$attr, Dtrain$intel)
logisticdata = na.omit(logisticdata)
colnames(logisticdata) = c("iid", "gender", "age_o", "race_o", "same_race","dec", "attr", "intel")

library(rpart)
fit <- rpart(as.factor(dec)~age_o + gender+ as.factor(race_o) + as.factor(same_race),
             method="class", data=logisticdata)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
print(fit)
summary(fit) # detailed summary of splits

# plot tree

plot(fit, uniform=TRUE, 
     main="Classification Tree for Decision")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
print(pfit)

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Decision")
#text(pfit, use.n=TRUE, all=TRUE, cex=.8)
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
#post(pfit, file = "c:/ptree.ps", 
 #    title = "Pruned Classification Tree for Decision")

# create attractive postscript plot of tree 
#post(fit, file = "c:/tree.ps", 
#     title = "Classification Tree for Decision")

test = data.frame(Dtest$iid, Dtest$gender, Dtest$age_o, Dtest$race_o, Dtest$samerace,Dtest$dec, Dtest$attr, Dtest$intel)
test = na.omit(test)
colnames(test) = c("iid", "gender", "age_o", "race_o", "same_race","dec", "attr", "intel")

pred <- predict(pfit, test)

pred = pred < 0.5
pred = pred[,1]

mean(pred == test$dec)

#############




##################################
#Dtrain = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$field_cd, Dtrain$attr_o,Dtrain$sinc_o,Dtrain$intel_o,Dtrain$fun_o,Dtrain$amb_o, Dtrain$attr3_1, Dtrain$sinc3_1, Dtrain$intel3_1, Dtrain$fun3_1, Dtrain$amb3_1, Dtrain$like_o, Dtrain$goal, Dtrain$date, Dtrain$go_out, Dtrain$exphappy, Dtrain$expnum)
#Dtrain = na.omit(Dtrain)
Dtrain = subset(data, !(wave %in% test))
Dtrain = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$field_cd, Dtrain$attr_o,Dtrain$sinc_o,Dtrain$intel_o,Dtrain$fun_o,Dtrain$amb_o, Dtrain$attr3_1, Dtrain$sinc3_1, Dtrain$intel3_1, Dtrain$fun3_1, Dtrain$amb3_1, Dtrain$goal, Dtrain$date, Dtrain$go_out, Dtrain$exphappy, Dtrain$like_o)
colnames(Dtrain) = c("iid", "gender", "age", "race", "field", "attr_o","sinc_o","intel_o","fun_o","amb_o", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1", "goal", "date", "go_out", "exphappy", "like_o")
Dtrain = na.omit(Dtrain)

Dtrain = aggregate(Dtrain[, 1:length(Dtrain)], list(Dtrain$iid), mean)
#now like_o is basically popularity

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
lambdas = 10^seq(10,-2,length = 100)
cv.out=cv.glmnet(X_train,y_train,alpha=1, lambda = lambdas)
bestlam=cv.out$lambda.min
lasso.mod=glmnet(X_train,y_train,alpha=1,lambda=bestlam)
lasso.coef=coef(lasso.mod)[,1]

#Calculating the test MSE
Dtest = aggregate(Dtest[, 1:length(Dtest)], list(Dtest$iid), mean)

X_te <- model.matrix(Dtest$like_o ~ Dtest$age + Dtest$gender + Dtest$age*Dtest$gender + as.factor(Dtest$race) + Dtest$gender*as.factor(Dtest$race) + as.factor(Dtest$field) + as.factor(Dtest$goal) + Dtest$date + Dtest$go_out + as.numeric(Dtest$exphappy))[,-1]
y_te = Dtest$like_o
pred.lasso = predict(lasso.mod, s = bestlam, newx = X_te)
mean((pred.lasso - y_te)**2)

#Later adding back in attractiveness and the other predictors

#Decision tree for classification of Yes or No
Dtrain = subset(data, !(wave %in% test))

DTrain = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$same_race, Dtrain$order, Dtrain$age_diff, Dtrain$int_corr, Dtrain$match)
colnames(Dtrain) = c("iid", "gender", "age", "race", "same_race", "order","age_diff","int_corr", "match")







###################################################################
##Logistic Regression 

##variables of interest:
## gender and race: your gender * your partner's race
## race: your race, your partner's race, your race * your partner's race, same race
## age: your age, your partner's age, your age * your partner's age
## interest correlation: int_corr
## your intentions: goal, date, go_out
## your expectations: exphappy


#creating the data frame
logisticdata = data.frame(Dtrain$dec, Dtrain$wave, Dtrain$gender, Dtrain$race, Dtrain$race_o, Dtrain$samerace, Dtrain$age, Dtrain$age_o,Dtrain$int_corr,Dtrain$goal,Dtrain$date,Dtrain$go_out, Dtrain$exphappy)
logisticdata = na.omit(logisticdata)
colnames(logisticdata) = c("dec","wave", "gender", "race", "race_o", "same_race","age","age_o","int_corr","goal","date","go_out","exphappy")

logisticdata_test = data.frame(Dtest$dec, Dtest$wave, Dtest$gender, Dtest$race, Dtest$race_o, Dtest$samerace, Dtest$age, Dtest$age_o,Dtest$int_corr,Dtest$goal,Dtest$date,Dtest$go_out, Dtest$exphappy)
logisticdata_test = na.omit(logisticdata_test)
colnames(logisticdata_test) = c("dec","wave", "gender", "race", "race_o", "same_race","age","age_o","int_corr","goal","date","go_out","exphappy")


#visualizing that women are more choosy
females = subset(logisticdata,logisticdata$gender ==0)
female_dec = females$dec
hist(female_dec)
males = subset(logisticdata,logisticdata$gender ==1)
male_dec = males$dec
hist(males$dec)

#backward selection
fullmodel = glm(as.factor(dec)~as.factor(gender)*as.factor(race_o) + as.factor(race_o)*as.factor(race) +  as.factor(same_race) + age + age_o + age*age_o + int_corr + as.factor(goal) + date + go_out + exphappy,family="binomial",data = logisticdata)
summary(fullmodel)
#exphappy = 0.71
model = glm(as.factor(dec)~as.factor(gender)*as.factor(race_o) + as.factor(race_o)*as.factor(race) +  as.factor(same_race) + age + age_o + age*age_o + int_corr + as.factor(goal) + date + go_out,family="binomial",data = logisticdata)
summary(model)
#your race 0.58
#age 0.62, age_o 0.869,age*age_o = 0.628
#int corr = 0.64   #getting rid of this 
model = glm(as.factor(dec)~as.factor(gender)*as.factor(race_o) + as.factor(race_o)*as.factor(race) +  as.factor(same_race) + age + age_o + age*age_o + as.factor(goal) + date + go_out,family="binomial",data = logisticdata)
summary(model)
#age = 0.61 # getting everything age
model = glm(as.factor(dec)~as.factor(gender)*as.factor(race_o) + as.factor(race_o)*as.factor(race) +  as.factor(same_race) + as.factor(goal) + date + go_out,family="binomial",data = logisticdata)
summary(model)
##nothing significant in race*otherrace
model = glm(as.factor(dec)~as.factor(gender)*as.factor(race_o) +  as.factor(same_race) + as.factor(goal) + date + go_out,family="binomial",data = logisticdata)
summary(model)

## here come the good models: ##################
## why isn't same race significant? fine i'll take it out
model = glm(as.factor(dec)~as.factor(gender)*as.factor(race_o) + as.factor(goal) + date + go_out ,family="binomial",data = logisticdata)
summary(model)
## ok date is decent, but i'll take it out just to have some models to compare 
model = glm(as.factor(dec)~as.factor(gender)*as.factor(race_o) + as.factor(goal) + go_out,family="binomial",data = logisticdata)
summary(model)



#cross validation for model = glm(as.factor(dec)~as.factor(gender)*as.factor(race_o) + as.factor(goal) + date + go_out,family="binomial",data = logisticdata)
fpr <- NULL
fnr <- NULL
acc <- NULL
train_waves = c(1,2,3,4,5,8,9,10,11,12,17,18,20,21)
for (i in train_waves){
  cv.index = which(logisticdata$wave == i)
  test = logisticdata[cv.index,]
  train = logisticdata[-cv.index,]
  model = model = glm(as.factor(dec)~as.factor(gender)*as.factor(race_o) + as.factor(goal) + date + go_out,family="binomial",data = train)
  results_prob <- predict(model,test,type='response')
  results <- ifelse(results_prob > 0.5,1,0)
  answers <- test$dec
  misClasificError <- mean(answers != results)
  acc = c(acc, 1-misClasificError)
  cm <- confusionMatrix(data=results, reference=answers)
  #fpr[i] <- cm$table[2]/(nrow(logisticdata)-smp_size)  #this code doesn't work for now
  #fnr[i] <- cm$table[3]/(nrow(logisticdata)-smp_size)  #this code doesn't work for now
}
avgmisClasificError = mean(misClasificError)
avgmisClasificError


