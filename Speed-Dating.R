#####################################################################################
## Data Visualization
#####################################################################################

library(ggplot2)
library(vioplot)
#Create age_diff
data = Speed_Dating_Data
age_diff = data$age - data$age_o
data = data.frame(data, age_diff)
Dtrain = data
Train = data.frame(Dtrain$iid, Dtrain$like,Dtrain$race, Dtrain$samerace,Dtrain$int_corr,Dtrain$attr,Dtrain$sinc,Dtrain$intel,Dtrain$fun,Dtrain$amb,Dtrain$shar,Dtrain$age_diff,Dtrain$like_o)
Train = na.omit(Train)
colnames(Train) = c("iid", "like","race", "samerace","int_corr","attr","sinc","intel","fun","amb","shar","age_diff", "like_o")

#Visualizing distribution of liking people
p <- ggplot(Train, aes(x=Train$like))
p + geom_histogram() + labs(x = "Like Score", y = "Frequency", title = "Distribution of Like Scores")
mean(Train$like)
sd(Train$like)


#Visualizing samerace's impact on like
par(mfrow=c(1,1))
p <- ggplot(Train, aes(as.factor(Train$samerace), as.numeric(Train$like)))
p + geom_boxplot()

#like score vs. same race violin plot
plot(0:1,0:1,type="n",axes=FALSE,ann=FALSE)
vioplot(Train.no.same.race$like,Train.yes.same.race$like,col="gray", names = c("",""))
title(main = "Like Score vs. Same Race", ylab = "Like Score")
axis(side=1,at=1:2,labels=c("Different Race","Same Race"))

#acctractive score vs. same race v,iolin plot
plot(0:1,0:1,type="n",axes=FALSE,ann=FALSE)
vioplot(Train.no.same.race$attr,Train.yes.same.race$attr,col="gray", names = c("",""))
title(main = "Attractive Score vs. Same Race", ylab = "Attractive Score")
axis(side=1,at=1:2,labels=c("Different Race","Same Race"))

#boxplots by race
#boxplot(Train$like~Train$samerace, xlab = "Same race, no on left")
#Black=subset(Train,Train$race==1)
#White=subset(Train,Train$race==2)
#Latino=subset(Train,Train$race==3)
#Asian=subset(Train,Train$race==4)
#Native=subset(Train,Train$race==5)
#boxplot(Latino$like~Latino$samerace, xlab = "Same race, no on left")

#Creating violin plots
library(vioplot)
#This funciton takes in predictor x and result y and makes
#violin plots with each value of x as a group for the plot.
#x and y must have 10 possible values
violin = function(x, y, xlabel, ylabel, title) {
  x1 = y[x==1]
  x2 = y[x==2]
  x3 = y[x==3]
  x4 = y[x==4]
  x5 = y[x==5]
  x6 = y[x==6]
  x7 = y[x==7]
  x8 = y[x==8]
  x9 = y[x==9]
  x10 = y[x==10]
  vioplot(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10, col="gray")
  title(main = title, xlab = xlabel, ylab = ylabel)
}

#Visualizing predictors with Violin plots
par(mfrow=c(2,3))

violin(Train$attr, Train$like,"Attractive Score","Like Score","Like Score Vs. Attractive Score")
violin(Train$intel, Train$like,"Intelligence Score","Like Score","Like Score Vs. Intelligence Score")
violin(Train$fun, Train$like,"Fun Score","Like Score","Like Score Vs. Fun Score")
violin(Train$amb, Train$like, "Ambition Score","Like Score","Like Score Vs. Ambition Score")
violin(Train$sinc, Train$like,"Sincerity Score","Like Score","Like Score Vs. Sincerity Score")
violin(Train$shar, Train$like,"Shared Interest Score","Like Score","Shared Interest Vs. Attractiveness Score")


#Percieved shared interest does not reflect actual shared interest
x1 = Train$shar[Train$int_corr<0]
x2 = Train$shar[Train$int_corr>0]
cor(Train$shar,Train$int_corr)
cor(Train$like,Train$int_corr)

#Correlation plot of all Attributes
install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1))

corrplot(cor(Train[-1]), tl.col = "black",method="number",main = "Correlation Plot")
corrplot(cor(Train[-1]), tl.col = "black",method="square",main = "Correlation Plot")

####################################################################################
##Linear Model on how much you like someone
####################################################################################

#Fitting a Model
library(MuMIn)
model = lm(Train$like~Train$age_diff+Train$samerace+Train$int_corr+Train$attr+Train$sinc+Train$intel+Train$fun+Train$amb, Train$shar,data=Train)
options(na.action = "na.fail")
dredge(model)[1]
#it looks like age_diff is the only predictor that isn't significant
model = lm(Train$like~ Train$amb + Train$attr + Train$fun + Train$int_corr + Train$intel + Train$samerace + Train$sinc)
summary(model)

####################################################################################
##Linear Model on how much you like someone using non-obvious predictors
####################################################################################

#Getting more variables 
AllData = data.frame(Dtrain$iid, Dtrain$like,Dtrain$age_o, Dtrain$age_diff,Dtrain$race_o, Dtrain$samerace,Dtrain$int_corr,Dtrain$like_o)
AllData = na.omit(AllData)
colnames(AllData) = c("iid","like","age_o","age_diff","race_o","samerace","int_corr","like_o")

#dredged model
global.model = lm(AllData$like~AllData$age_o+AllData$age_diff+AllData$race_o+AllData$samerace+AllData$int_corr)
dredge(global.model)[1]
summary(lm(AllData$like~AllData$age_diff+AllData$race_o+AllData$samerace+AllData$int_corr))

####################################################################################
##Linear Model on how much you like someone using non-obvious predictors, split data
####################################################################################

#Create test set: setting aside 7 waves for test set
set.seed(6)
set_aside = sample(21,7)
Test_Data = subset(data, wave %in% test)
#Create training set: 14 waves in training set
Training_Data = subset(data, !(wave %in% test))

#Limiting to the variables we need
Training_Data = data.frame(Training_Data$like,Training_Data$age_o, Training_Data$age_diff,Training_Data$race_o, Training_Data$samerace,Training_Data$int_corr)
Training_Data = na.omit(Training_Data)
colnames(Training_Data) = c("like","age_o","age_diff","race_o","samerace","int_corr")
Test_Data = data.frame( Test_Data$like,Test_Data$age_o, Test_Data$age_diff,Test_Data$race_o, Test_Data$samerace,Test_Data$int_corr)
Test_Data = na.omit(Test_Data)
colnames(Test_Data) = c("like","age_o","age_diff","race_o","samerace","int_corr")

library(leaps)
regfit.full=regsubsets(Training_Data$like~.,Training_Data)
summary(regfit.full)
reg.summary=summary(regfit.full)
reg.summary$rsq

#some plots about subsets
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red",cex=2,pch=20)

#cross validation to pick the best number of predictors
regfit.best=regsubsets(Training_Data$like~.,data=Training_Data,nvmax=5)
test.mat=model.matrix(Test_Data$like~.,data=Test_Data) # create an X matrix of test data
val.errors=rep(NA,5)
for(i in 1:5){
  coefi=coef(regfit.best,id=i)
  print(coefi)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Test_Data$like-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,1)
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
model = regsubsets(Training_Data$like~.,data=Training_Data,nvmax=5)

#####################################################################################
## Linear Regression: Demographic Information and Self-Perception and visualizations
#####################################################################################
Train2 = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$field_cd, Dtrain$attr_o,Dtrain$sinc_o,Dtrain$intel_o,Dtrain$fun_o,Dtrain$amb_o, Dtrain$attr3_1, Dtrain$sinc3_1, Dtrain$intel3_1, Dtrain$fun3_1, Dtrain$amb3_1, Dtrain$like_o)
Train2 = na.omit(Train2)
colnames(Train2) = c("iid", "gender", "age", "race", "field", "attr_o","sinc_o","intel_o","fun_o","amb_o", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1", "like_o")

#now each row is a person, not an interaction
Train2 = aggregate(Train2[, 1:length(Train2)], list(Train2$iid), mean)
#Train2$self_awar = Train2[7:11]-Train2[12:16]  #differencing variable
#creating the self_perc_score variable, correlation variable
for (i in 1:nrow(Train2)){

  x = c(Train2[i,]$attr_o,Train2[i,]$sinc_o,Train2[i,]$intel_o,Train2[i,]$fun_o,Train2[i,]$amb_o)
  y = c(Train2[i,]$attr3_1,Train2[i,]$sinc3_1,Train2[i,]$intel3_1,Train2[i,]$fun3_1,Train2[i,]$amb3_1)
  Train2$self_awar_score[i] = mean(x-y)
}

#who gets an average like of 7.5&up
Popular = subset(Train2,Train2$like_o>7.65)
#who gets an average of 2.5
Unpopular = subset(Train2,Train2$like_o<4.57)
table(Popular$race, Popular$gender)
table(Unpopular$race, Unpopular$gender)

#ommitting all nas
Train2 = na.omit(Train2)

#visualizing self_awar_score
p <- ggplot(Train2, aes(x=Train2$self_awar_score))
p + geom_histogram() + labs(x = "Self Awareness Score", y = "Frequency", title = "Distribution of Self Awareness Scores")
mean(Train2$self_awar_score)
sd(Train2$self_awar_score)

#visualizing self_awar_score with gender, age, race, field
par(mfrow=c(2,2))

p1 <- ggplot(Train2, aes(as.factor(Train2$gender),Train2$self_awar_score))
p1 + geom_boxplot() +  labs(y = "Self Awareness Score", x = "Gender", title =  "Self Awareness Score by Gender")  + scale_x_discrete(labels=c("Female", "Male"))

p2 <- ggplot(Train2, aes(as.factor(Train2$race),Train2$self_awar_score))
p2 + geom_boxplot() + labs(y = "Self Awareness Score", x = "Race", title =  "Self Awareness Score by Race")  + scale_x_discrete(labels=c("Black/African American", "European/Caucasian-American", "Latino/Hispanic American", "Asian/Pacific Islander/Asian-American","Other"))  + theme(axis.text.x = element_text(angle=10))

p3 <- ggplot(Train2, aes(as.factor(Train2$field),Train2$self_awar_score))
p3 + geom_boxplot() + labs(y = "Self Awareness Score", x = "Field", title =  "Self Awareness Score by Field") + scale_x_discrete(labels=c("Law", "Math", "Social Science", "Medical Science", "Engineering","English","History","Business","Education","Biological Sciences","Social Work","Undergrad","Political Science","Film","Fine Arts","Languages","Architecture","Other"))  + theme(axis.text.x = element_text(angle=30))

p4 <- ggplot(Train2, aes(as.factor(Train2$age),Train2$self_awar_score))
p4 + geom_point() + labs(y = "Self Awareness Score", x = "Age", title =  "Self Awareness Score by Age")  

#fit linear model
model = lm(Train2$self_awar_score~as.factor(Train2$gender) + Train2$age + as.factor(Train2$race) + as.factor(Train2$field))
options(na.action = "na.fail")
dredge(model)[1]
model = lm(Train2$self_awar_score~as.factor(Train2$gender) + as.factor(Train2$race))
model = lm(Train2$self_awar_score~ as.factor(Train2$race))
model = lm(Train2$self_awar_score~ as.factor(Train2$gender))
model = lm(Train2$self_awar_score~ as.factor(Train2$race))
summary(model)

#does one's self-awarness predict how much other people like you?
par(mfrow=c(1,1))
plot(Train2$self_awar_score,Train2$like_o) #I think this is quadratic!!!!! (downward, centered on 0)
hist(Train2$like_o)

#this is using what I expect others to think of me
Train3 = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$field_cd, Dtrain$attr_o,Dtrain$sinc_o,Dtrain$intel_o,Dtrain$fun_o,Dtrain$amb_o, Dtrain$attr3_1, Dtrain$sinc3_1, Dtrain$intel3_1, Dtrain$fun3_1, Dtrain$amb3_1, Dtrain$attr5_1, Dtrain$sinc5_1, Dtrain$intel5_1, Dtrain$fun5_1, Dtrain$amb5_1)
colnames(Train3) = c("iid", "gender", "age", "race", "field", "attr_o","sinc_o","intel_o","fun_o","amb_o", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1", "attr5_1", "sinc5_1", "intel5_1", "fun5_1", "amb5_1")
Train3 = na.omit(Train3)

for (i in 1:nrow(Train3)){
  x = c(Train3[i,]$attr_o,Train3[i,]$sinc_o,Train3[i,]$intel_o,Train3[i,]$fun_o,Train3[i,]$amb_o)
  y = c(Train3[i,]$attr3_1,Train3[i,]$sinc3_1,Train3[i,]$intel3_1,Train3[i,]$fun3_1,Train3[i,]$amb3_1)
  z = c(Train3[i,]$attr5_1,Train3[i,]$sinc5_1,Train3[i,]$intel5_1,Train3[i,]$fun5_1,Train3[i,]$amb5_1)
  Train3$self_awar_score[i] =  mean(x-y)
  Train3$exp_self_awar_score[i] =  mean(x-z)
}

hist(Train3$self_awar_score)
hist(Train3$exp_self_awar_score)

t.test(Train3$self_awar_score,Train3$exp_self_awar_score,paired=TRUE)
#no difference between self-awareness score and how they expect others to perceive them




