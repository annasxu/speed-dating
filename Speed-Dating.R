
Speed_Dating_Data <- read.csv("~/Desktop/speed-dating-experiment/Speed Dating Data.csv")



data = Speed_Dating_Data
age_diff = data$age - data$age_o
data = data.frame(data, age_diff)

#setting aside 7 waves for test set
set.seed(6)
test = sample(21,7)
Dtest = subset(data,wave %in% test)

#14 waves in training set
Dtrain = subset(data, !(wave %in% test))


Dtrain_like = data.frame(Dtrain$like,Dtrain$samerace,Dtrain$int_corr,Dtrain$attr,Dtrain$sinc+Dtrain$intel,Dtrain$fun,Dtrain$amb,Dtrain$shar)
Dtrain_like = na.omit(Dtrain_like)
Dtrain_like$Dtrain.attr

model = lm(Dtrain$like~Dtrain$age_diff+Dtrain$samerace+Dtrain$int_corr+Dtrain$attr+Dtrain$sinc+Dtrain$intel+Dtrain$fun+Dtrain$amb+Dtrain$shar,data=Dtrain)
summary(model)



plot(Dtrain$int_corr,Dtrain$like)
as.factor(Dtrain_like$Dtrain.attr)
as.factor(Dtrain_like$Dtrain.like)
plot(Dtrain_like$Dtrain.attr,Dtrain_like$Dtrain.like)

plot(Dtrain_like$Dtrain.attr,Dtrain_like$Dtrain.like)

bagplot(Dtrain_like$Dtrain.attr,Dtrain_like$Dtrain.like)


library(vioplot)
x1 = Dtrain_like$Dtrain.like[Dtrain_like$Dtrain.attr==1]
x2 = Dtrain_like$Dtrain.like[Dtrain_like$Dtrain.attr==2]
x3 = Dtrain_like$Dtrain.like[Dtrain_like$Dtrain.attr==3]
x4 = Dtrain_like$Dtrain.like[Dtrain_like$Dtrain.attr==4]
x5 = Dtrain_like$Dtrain.like[Dtrain_like$Dtrain.attr==5]
x6 = Dtrain_like$Dtrain.like[Dtrain_like$Dtrain.attr==6]
x7 = Dtrain_like$Dtrain.like[Dtrain_like$Dtrain.attr==7]
x8 = Dtrain_like$Dtrain.like[Dtrain_like$Dtrain.attr==8]
x9 = Dtrain_like$Dtrain.like[Dtrain_like$Dtrain.attr==9]
x10 = Dtrain_like$Dtrain.like[Dtrain_like$Dtrain.attr==10]
vioplot(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)

