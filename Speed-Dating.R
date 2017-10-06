Speed_Dating_Data = read.csv("Speed Dating Data.csv")

data =Speed_Dating_Data
data = data.frame(data, age_diff)

#setting aside 7 waves for test set
set.seed(6)
test = sample(21,7)
Dtest = subset(data,wave %in% test)

#14 waves in training set
Dtrain = subset(data, !(wave %in% test))

model = lm(Speed_Dating_Data$like~Speed_Dating_Data$samerace+Speed_Dating_Data$int_corr+Speed_Dating_Data$attr+Speed_Dating_Data$sinc+Speed_Dating_Data$intel+Speed_Dating_Data$fun+Speed_Dating_Data$amb+Speed_Dating_Data$shar,data=data)
summary(model)

model2 = lm(Speed_Dating_Data$like~Speed_Dating_Data$samerace+Speed_Dating_Data$int_corr+Speed_Dating_Data$attr+Speed_Dating_Data$sinc+Speed_Dating_Data$intel+Speed_Dating_Data$fun+Speed_Dating_Data$amb,data=data)
summary(model2)