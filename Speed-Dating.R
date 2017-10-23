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
Train = data.frame(Dtrain$iid, Dtrain$like,Dtrain$race, Dtrain$samerace,Dtrain$int_corr,Dtrain$attr,Dtrain$sinc,Dtrain$intel,Dtrain$fun,Dtrain$amb,Dtrain$shar,Dtrain$age_diff)
Train = na.omit(Train)
colnames(Train) = c("iid", "like","race", "samerace","int_corr","attr","sinc","intel","fun","amb","shar","age_diff")

#Visualizing distribution of liking people
p <- ggplot(Train, aes(x=Train$like))
p + geom_histogram() + labs(x = "Like Score", y = "Frequency", title = "Distribution of Like Scores")


#Visualizing samerace's impact on like
par(mfrow=c(1,1))
p <- ggplot(Train, aes(as.factor(Train$samerace), as.numeric(Train$like)))
p + geom_boxplot()

boxplot(Train$samerace,Train$like)

Train.no.same.race <- subset(Train, Train$samerace == 0)  
Train.yes.same.race <- subset(Train, Train$samerace == 1)  
mean(Train.no.same.race$like)
hist(Trian.no.same.race$like)
mean(Train.yes.same.race$like)


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

#Visualizing Interest Correlation with Shared Interest and Liking
plot(Train$int_corr,Train$shar)
plot(Train$int_corr,Train$like)
#Percieved shared interest does not reflect actual shared interest
x1 = Train$shar[Train$int_corr<0]
x2 = Train$shar[Train$int_corr>0]
vioplot(x1,x2,col="gray")
cor(Train$shar,Train$int_corr)
cor(Train$like,Train$int_corr)

#Correlation plot of all Attributes
install.packages("corrplot")
library(corrplot)
par(mfrow=c(1,1))
corrplot(cor(Train[-1]), tl.col = "black",method="number",main = "Correlation Plot")

#Fitting a Model
library(MuMIn)
model = lm(Train$like~Train$age_diff+Train$samerace+Train$int_corr+Train$attr+Train$sinc+Train$intel+Train$fun+Train$amb, Train$shar,data=Train)
options(na.action = "na.fail")
dredge(model)[1]
model = lm(Train$like~Train$age_diff + Train$amb + Train$attr + Train$fun + Train$int_corr + Train$intel + Train$samerace + Train$sinc)
summary

model = lm(Train$like~ Train$amb + Train$attr + Train$fun + Train$int_corr + Train$intel + Train$samerace + Train$sinc)
summary(model)


#####################################################################################
## Linear Regression: Demographic Information and Self-Perception
#####################################################################################
Train2 = data.frame(Dtrain$iid, Dtrain$gender, Dtrain$age, Dtrain$race, Dtrain$field_cd, Dtrain$attr,Dtrain$sinc,Dtrain$intel,Dtrain$fun,Dtrain$amb, Dtrain$attr3_1, Dtrain$sinc3_1, Dtrain$intel3_1, Dtrain$fun3_1, Dtrain$amb3_1, Dtrain$like_o)
Train2 = na.omit(Train2)
colnames(Train2) = c("iid", "gender", "age", "race", "field", "attr","sinc","intel","fun","amb", "attr3_1", "sinc3_1", "intel3_1", "fun3_1", "amb3_1", "like_o")

#now each row is a person, not an interaction
Train2 = aggregate(Train2[, 1:length(Train2)], list(Train2$iid), mean)
#Train2$self_awar = Train2[7:11]-Train2[12:16]  #differencing variable
#creating the self_perc_score variable, correlation variable
for (i in 1:nrow(Train2)){

  x = c(Train2[i,]$attr,Train2[i,]$sinc,Train2[i,]$intel,Train2[i,]$fun,Train2[i,]$amb)
  y = c(Train2[i,]$attr3_1,Train2[i,]$sinc3_1,Train2[i,]$intel3_1,Train2[i,]$fun3_1,Train2[i,]$amb3_1)
  Train2$self_awar_score[i] =  (cor(x,y))
}


#ommitting all nas
Train2 = na.omit(Train2)

#visualizing self_awar_score
p <- ggplot(Train2, aes(x=Train2$self_awar_score))
p + geom_histogram() + labs(x = "Self Awareness Score", y = "Frequency", title = "Distribution of Self Awareness Scores")


#visualizing self_awar_score with gender, age, race, field
par(mfrow=c(2,2))

p1 <- ggplot(Train2, aes(as.factor(Train2$gender),Train2$self_awar_score))
p1 + geom_boxplot() + labs(y = "Self Awareness Score", x = "Gender", title =  "Self Awareness Score by Gender")  + scale_x_discrete(labels=c("Female", "Male"))

p2 <- ggplot(Train2, aes(as.factor(Train2$race),Train2$self_awar_score))
p2 + geom_boxplot() + labs(y = "Self Awareness Score", x = "Race", title =  "Self Awareness Score by Race")  + scale_x_discrete(labels=c("Black/African American", "European/Caucasian-American", "Latino/Hispanic American", "Asian/Pacific Islander/Asian-American", "Native American","Other"))  + theme(axis.text.x = element_text(angle=10))

p3 <- ggplot(Train2, aes(as.factor(Train2$field),Train2$self_awar_score))
p3 + geom_boxplot() + labs(y = "Self Awareness Score", x = "Field", title =  "Self Awareness Score by Field") 


p4 <- ggplot(Train2, aes(as.factor(Train2$age),Train2$self_awar_score))
p4 + geom_point() + labs(y = "Self Awareness Score", x = "Age", title =  "Self Awareness Score by Age")  


#not using self_awar_score, using attr diff.
Train2$attr_diff = Train2$attr-Train2$attr3_1
par(mfrow=c(1,2))
boxplot(Train2$attr_diff~Train2$gender,data = Train2)
boxplot(Train2$gender,Train2$attr_diff,data = Train2)

#fit linear model
model = lm(Train2$self_awar_score~as.factor(Train2$gender) + Train2$age + as.factor(Train2$race) + as.factor(Train2$field))
options(na.action = "na.fail")
dredge(model)[1]
model = lm(Train2$self_awar_score~as.factor(Train2$race))
summary(model)


#does one's self-awarness predict how much other people like you?
plot(Train2$self_awar_score,Train2$like_o)


#this is using what I expect others to think of me
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

t.test(Train3$self_awar_score,Train3$exp_self_awar_score,paired=TRUE)
#no difference between self-awareness score and how they expect others to perceive them



