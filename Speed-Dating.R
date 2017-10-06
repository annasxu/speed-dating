Speed_Dating_Data = read.csv("Speed Dating Data.csv")

data =Speed_Dating_Data
age_diff = data$age - data$age_o
data = data.frame(data, age_diff)

#setting aside 7 waves for test set
set.seed(6)
test = sample(21,7)
Dtest = subset(data,wave %in% test)

#14 waves in training set
Dtrain = subset(data, !(wave %in% test))

plot(Dtrain$age_diff,Dtrain$)