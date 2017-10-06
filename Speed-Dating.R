Speed_Dating_Data <- read_csv("~/Desktop/speed-dating-experiment/Speed Dating Data.csv")
data =data.frame(Speed_Dating_Data$like,Speed_Dating_Data$samerace,Speed_Dating_Data$int_corr)

data = na.omit(data)
data

model = lm(Speed_Dating_Data$like~Speed_Dating_Data$samerace+Speed_Dating_Data$int_corr+Speed_Dating_Data$attr+Speed_Dating_Data$sinc+Speed_Dating_Data$intel+Speed_Dating_Data$fun+Speed_Dating_Data$amb+Speed_Dating_Data$shar,data=data)
summary(model)