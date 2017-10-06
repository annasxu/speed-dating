<<<<<<< HEAD
Speed_Dating_Data = read.csv("Speed Dating Data.csv")
=======
Speed_Dating_Data <- read.csv("~/Desktop/speed-dating-experiment/Speed Dating Data.csv")
>>>>>>> 76b7e572c99ccf02c2cffc71f3ac5d09e7f7176d
data =data.frame(Speed_Dating_Data$like,Speed_Dating_Data$samerace,Speed_Dating_Data$int_corr)

data = na.omit(data)
data

model = lm(Speed_Dating_Data$like~Speed_Dating_Data$samerace+Speed_Dating_Data$int_corr+Speed_Dating_Data$attr+Speed_Dating_Data$sinc+Speed_Dating_Data$intel+Speed_Dating_Data$fun+Speed_Dating_Data$amb+Speed_Dating_Data$shar,data=data)
summary(model)
<<<<<<< HEAD
=======

model2 = lm(Speed_Dating_Data$like~Speed_Dating_Data$samerace+Speed_Dating_Data$int_corr+Speed_Dating_Data$attr+Speed_Dating_Data$sinc+Speed_Dating_Data$intel+Speed_Dating_Data$fun+Speed_Dating_Data$amb,data=data)
summary(model2)
>>>>>>> 76b7e572c99ccf02c2cffc71f3ac5d09e7f7176d
