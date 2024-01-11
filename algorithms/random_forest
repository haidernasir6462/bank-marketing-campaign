# Step 1 # FirstCode
# Load marketing data
Mydata <- bank.additional
summary(Mydata)
barplot(table(bank.additional$y))
# Remove variable "duration"
Mydata <- subset(Mydata, select = -c(duration) )
# Split data into training (80%) and validation (20%)
A <- sort(sample(nrow(Mydata), 
                 nrow(Mydata)*.80))
Train <- Mydata[A,] # train data
Val <- Mydata[-A,] # validation data
# Step 2
# load the library randomForest
library(randomForest)
# build a Random Forest of 100 DTs with outcome y on the train data

#This section builds a random forest model with 100 decision trees (ntree=100). 
#It predicts the target variable y based on all other variables in the training data. 
#The model is set to use 10 variables randomly sampled at each split (mtry=10). 
#Importance of variables is computed and stored.
rf <- randomForest(as.factor(y) ~ ., data = Train,
                   ntree=50, 
                   importance=TRUE,
                   mtry=15, 
                   sampsize=c(800,400))
print(rf)
plot(rf)
# plot importance of variables
varImpPlot(rf)
# Step 3
Yt=predict(rf,Val)# predict results on the validation data
conf.matrix <- table(Val$y, Yt) # build a confusion matrix
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix))
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix))
print(conf.matrix)
acc=mean(Val$y==Yt) # accuracy
tp=sum(Val$y=='yes' & Yt=='yes')/sum(Val$y=='yes') # true positive rate
tn=sum(Val$y=='no' & Yt=='no')/sum(Val$y=='no') # true negative rate
fp=sum(Val$y=='no' & Yt=='yes')/sum(Val$y=='no') # false positive rate
fn=sum(Val$y=='yes' & Yt=='no')/sum(Val$y=='yes') # false negative rate
sprintf("Accuracy:%.2f, TP:%.2f, TN:%.2f",acc,tp,tn)# formatted printing
sprintf("Error rates: FP:%.2f, FN:%.2f",fp,fn)