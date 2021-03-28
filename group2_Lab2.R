#abalone
# read data in
aba<-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"),header=TRUE, sep=',')
colnames(aba)<-c("Sex","Length","Diameter","Height","Whole.weight","Shucked.weight","Viscera.weight","Shell.weight","Rings")
View(aba)
naba<-dim(aba)[1]
#90% to train
sampling.rate=0.9
#remainder to test
num.test.set.labels=naba*(1.-sampling.rate)
#construct a random set of training indices (training)
training <-sample(1:naba,sampling.rate*naba, replace=FALSE)
#build the training set (train)
train<-subset(aba[training,],select=c("Sex","Length","Diameter","Height","Whole.weight","Shucked.weight","Viscera.weight","Shell.weight"))
#construct the remaining test indices (testing)
testing<-setdiff(1:naba,training)
#define the test set
test<-subset(aba[testing,],select=c("Sex","Length","Diameter","Height","Whole.weight","Shucked.weight","Viscera.weight","Shell.weight"))
#construct labels for another variable (Rings) in the training set
crings<-aba$Rings[training]
#construct true labels the other variable in the test set
true.labels<-aba$Rings[testing]
#run the classifier, can change k
classif<-knn(train,test,crings,k=5)
#view the classifier
classif
#looks at attributes
attributes(.Last.value)

#kknn1
#compare to kknn?

library(kknn)
spam.kknn <- kknn(spam~., train, test, distance =1, kernel = "triangular")
summary(spam.kknn)
# etc....
# other distances and kernels!!

#nbayes1
require(mlbench)
data(HouseVotes84)
View(HouseVotes84)
library(e1071) #using library e1071
model <- naiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,-1])
predict(model, HouseVotes84[1:10,-1], type = "raw")

pred <- predict(model, HouseVotes84[,-1])
table(pred, HouseVotes84$Class)

## Example of using a contingency table:
data(Titanic)
View(Titanic)
m <- naiveBayes(Survived ~ ., data = Titanic)
m
predict(m, as.data.frame(Titanic)[,1:3])

## Example with metric predictors:
data(iris)
View(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternatively:
m <- naiveBayes(iris[,-5], iris[,5])
m
table(predict(m, iris[,-5]), iris[,5])

#nbayes2
data(Titanic)
mdl <- naiveBayes(Survived ~ ., data = Titanic)
mdl
# etc.

#nbayes3
require(mlbench)
data(HouseVotes84)
library(klaR) #with library klaR
model <- NaiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,-1])

pred <- predict(model, HouseVotes84[,-1])
table(pred$class, HouseVotes84$Class)

#nbayes4
