#####################################################################################
## Linear Regression: Scored Attributes and Liking
#####################################################################################

#Create age_diff
data = Speed_Dating_Data
age_diff = data$age - data$age_o
data = data.frame(data, age_diff)

#Create test set: setting aside 7 waves for test set
set.seed(6)
test = sample(21,7)
Dtest = subset(data,wave %in% test)

#Create training set: 14 waves in training set
Dtrain = subset(data, !(wave %in% test))
Train = data.frame(Dtrain$iid, Dtrain$like,Dtrain$samerace,Dtrain$int_corr,Dtrain$attr,Dtrain$sinc,Dtrain$intel,Dtrain$fun,Dtrain$amb,Dtrain$shar,Dtrain$age_diff)
Train = na.omit(Train)
colnames(Train) = c("iid", "like","samerace","int_corr","attr","sinc","intel","fun","amb","shar","age_diff")

#Visualizing distribution of liking people
hist(Train$like)

#Visualizing samerace's impact on like
boxplot(Train$samerace,Train$like, xlab = "Same race, no on left")


#Creating violin plots
library(vioplot)
#This funciton takes in predictor x and result y and makes
#violin plots with each value of x as a group for the plot.
#x and y must have 10 possible values
violin = function(x, y) {
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
  vioplot(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
}

#Visualizing predictors with Violin plots
violin(Train$attr, Train$like)
violin(Train$intel, Train$like)
violin(Train$fun, Train$like)
violin(Train$amb, Train$like)
violin(Train$sinc, Train$like)
violin(Train$shar, Train$like)

#Visualizing Interest Correlation with Shared Interest and Liking
plot(Train$int_corr,Train$shar)
plot(Train$int_corr,Train$like)
#Percieved shared interest does not reflect actual shared interest
x1 = Train$shar[Train$int_corr<0]
x2 = Train$shar[Train$int_corr>0]
vioplot(x1,x2)
cor(Train$shar,Train$int_corr)
cor(Train$like,Train$int_corr)

#Correlation plot of all Attributes
install.packages("corrplot")
library(corrplot)
corrplot(cor(Train[-1]), tl.col = "black",method="number")

#Fitting a Model
model = lm(Train$like~Train$age_diff+Train$samerace+Train$int_corr+Train$attr+Train$sinc+Train$intel+Train$fun+Train$amb, Train$shar,data=Train)
summary(model)


#####################################################################################
## Linear Regression: Scored Attributes and Self-Perception
#####################################################################################
Train2 = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$field_cd, Dtrain$attr,Dtrain$sinc,Dtrain$intel,Dtrain$fun,Dtrain$amb, Dtrain$attr3_1, Dtrain$sinc3_1, Dtrain$intel3_1, Dtrain$fun3_1, Dtrain$amb3_1)
Train2 = na.omit(Train2)
colnames(Train2) = c("iid", "gender", "age", "race", "field", "attr","sinc","intel","fun","amb", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1")


Train2 = aggregate(Train2[, 1:15], list(Train2$iid), mean)
#Train2$self_awar = Train2[7:11]-Train2[12:16]  #differencing variable
#creating the self_perc_score variable, correlation variable
for (i in 1:nrow(Train2)){

  x = c(Train2[i,]$attr,Train2[i,]$sinc,Train2[i,]$intel,Train2[i,]$fun,Train2[i,]$amb)
  y = c(Train2[i,]$attr3_1,Train2[i,]$sinc3_1,Train2[i,]$intel3_1,Train2[i,]$fun3_1,Train2[i,]$amb3_1)
  Train2$self_awar_score[i] =  (cor(x,y))
}


#ommitting all nas
Train2 = na.omit(Train2)

#visualizing self_perc_score and gender
hist(Train2$self_awar_score)


#visualizing self_perc_score with all 
boxplot(Train2$gender,Train2$self_awar_score)
Train2$race = factor(Train2$race)
boxplot(Train2$self_awar_score,Train2$race,data = Train2)

#not using self_awar_score, using attr diff.
boxplot(Train2$gender,Train2$attr-Train2$attr3_1,data = Train2)
Train2$attr_diff = Train2$attr-Train2$attr3_1
gender0 <- subset(Train2, Train2$gender == 0)
gender1 <- subset(Train2, Train2$gender == 1)
var(gender0$attr_diff)
var(gender1$attr_diff)
hist(gender0$attr_diff)
hist(gender1$attr_diff)



#this is using what I expect other to think of me
Train3 = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$field_cd, Dtrain$attr,Dtrain$sinc,Dtrain$intel,Dtrain$fun,Dtrain$amb, Dtrain$attr3_1, Dtrain$sinc3_1, Dtrain$intel3_1, Dtrain$fun3_1, Dtrain$amb3_1, Dtrain$attr5_1, Dtrain$sinc5_1, Dtrain$intel5_1, Dtrain$fun5_1, Dtrain$amb5_1)
colnames(Train3) = c("iid", "gender", "age", "race", "field", "attr","sinc","intel","fun","amb", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1", "attr5_1", "sinc5_1", "intel5_1", "fun5_1", "amb5_1")
Train3 = na.omit(Train3)

for (i in 1:nrow(Train3)){
  
  x = c(Train3[i,]$attr,Train3[i,]$sinc,Train3[i,]$intel,Train3[i,]$fun,Train3[i,]$amb)
  y = c(Train3[i,]$attr3_1,Train3[i,]$sinc3_1,Train3[i,]$intel3_1,Train3[i,]$fun3_1,Train3[i,]$amb3_1)
  z = c(Train3[i,]$attr5_1,Train3[i,]$sinc5_1,Train3[i,]$intel5_1,Train3[i,]$fun5_1,Train3[i,]$amb5_1)
  Train3$self_awar_score[i] =  (cor(x,y))
  Train3$exp_self_awar_score[i] =  (cor(x,z))
}


View(Train3)
hist(Train3$self_awar_score)
hist(Train3$exp_self_awar_score)






