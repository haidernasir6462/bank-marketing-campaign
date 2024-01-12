require(gbm)
require(MASS)
str(Boston)
summary(Boston)
train=sample(1:506,size=374)
Boston.boost = gbm(medv ~ ., data = Boston[train,], 
                   distribution = "gaussian", 
                   n.trees = 10000, 
                   shrinkage = 0.01, 
                   interaction.depth = 4)
Boston.boost
summary(Boston.boost)
plot(Boston.boost,i="lstat") 
plot(Boston.boost,i="rm")
n.trees = seq(from=100 ,to=10000, by=100)
predmatrix<-predict(Boston.boost, Boston[-train,], n.trees = n.trees)
dim(predmatrix)
Test.error <- with(Boston[-train,], apply( (predmatrix-medv)^2,2, mean))
head(test.error)
head(Test.error)
plot(n.trees , Test.error, pch=23,col="blue", xlab="Number of Trees", ylab="Test Error", main="Performance of Boosting on Test Set")
abline(h = min(test.err),col="red") 
abline(h = min(Test.error),col="red")
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)