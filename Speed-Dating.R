data = Speed_Dating_Data
age_diff = data$age - data$age_o
data = data.frame(data, age_diff)

#setting aside 7 waves for test set
set.seed(6)
test = sample(21,7)
Dtest = subset(data,wave %in% test)

#14 waves in training set
Dtrain = subset(data, !(wave %in% test))

Train = data.frame(Dtrain$like,Dtrain$samerace,Dtrain$int_corr,Dtrain$attr,Dtrain$sinc,Dtrain$intel,Dtrain$fun,Dtrain$amb,Dtrain$shar,Dtrain$age_diff)
Train = na.omit(Train)
colnames(Train) = c("like","samerace","int_corr","attr","sinc","intel","fun","amb","shar","age_diff")

#number of NAs for like = 10, like = 9, like = 8
#[explanation here]



#race visualization
boxplot(Train$samerace,Train$like, xlab = "Same race, no on left")

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

violin(Train$attr, Train$like)

#histogram of liking people
hist(Train$like)

#correlation plot 
#install.packages("corrplot")
library(corrplot)
corrplot(cor(Train[-1]), tl.col = "black",method="number")



#
model = lm(Train$like~Train$age_diff+Train$samerace+Train$int_corr+Train$attr+Train$sinc+Train$intel+Train$fun+Train$amb, Train$shar,data=Train)
summary(model)


model = lm(Train$like~Train$samerace)
summary(model)





