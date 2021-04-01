View(Titanic)
summary(Titanic)
data("Titanic")
dim(Titanic)

#rpart
library(rpart)
dectionTreeModel <- rpart(Survived~., data=Titanic,method="class")
rpart.plot::rpart.plot(dectionTreeModel)

#ctree
require(party)
titanic_tree <- ctree(Survived~., data=Titanic)
plot(titanic_tree)

#hclust
set.seed(145)
par(mar=rep(0.2,4))
df<-scale(Titanic)
head(df)
d <- dist(df, method = "euclidean")
hc1 <- hclust(d, method="complete")
plot(hc1, cex=0.6, hang=-1)

#Random Forest
library(randomForest)
set.seed(100)
nrow(Titanic)
train<-sample(nrow(Titanic),0.7*nrow(Titanic),replace=F)
train
Trainset<-Titanic[train,,,]
Testset<-Titanic[-train,,,]
View(Trainset)
View(Testset)
model1<- randomForest(Survived~., data=Trainset, importance=T)
model1
varImpPlot(model1)
predTrain <- predict(model1, Trainset[,-4,,], type='class')
table(predTrain, Trainset[,4,,])

predTest <- predict(model1, Testset, type='class')
table(predTest, Testset$Survived)
