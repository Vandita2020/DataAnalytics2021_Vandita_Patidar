data("iris")
head("iris")
irisdata1<-iris[,1:4]
irisdata1
head(irisdata1)
help("princomp")
principal_components<-princomp(irisdata1,cor=TRUE,score=TRUE)
# cor = a logical value indicating whether the calculation should 
#use the correlation matrix or the covariance matrix.
# (The correlation matrix can only be used if there are no constant variables.)
# score = a logical value indicating whether the score on  
# each principal component should be calculated.
summary(principal_components)
plot(principal_components)
# plotting the principal_components using the a line in plot() functions 
plot(principal_components,type='l')
help("biplot")
biplot(principal_components)
