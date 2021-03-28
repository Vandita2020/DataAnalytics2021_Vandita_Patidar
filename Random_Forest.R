require(randomForest)
library(rpart)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fitKF) 	# view results

importance(fitKF) # importance of each predictor


