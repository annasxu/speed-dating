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

boxplot(Train$samerace,Train$like, xlab = "Same race, no on left")

plot(Train$int_corr,Train$like)
as.factor(Train$attr)
as.factor(Train$like)
plot(Train$attr,Train$like)

plot(Train$attr,Train$like)

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
violin(Train$intel, Train$like)
violin(Train$fun, Train$like)
violin(Train$amb, Train$like)
violin(Train$sinc, Train$like)
violin(Train$shar, Train$like)

plot(Train$int_corr,Train$shar)
plot(Train$int_corr,Train$like)

#percieved shared interest does not reflect actual shared interest
x1 = Train$shar[Train$int_corr<0]
x2 = Train$shar[Train$int_corr>0]
vioplot(x1,x2)
cor(Train$shar,Train$int_corr)
cor(Train$like,Train$int_corr)

grp1 = mean()
barplot()

model = lm(Train$like~Train$age_diff+Train$samerace+Train$int_corr+Train$attr+Train$sinc+Train$intel+Train$fun+Train$amb, Train$shar,data=Train)
summary(model)
