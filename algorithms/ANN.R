#visualize structure of dataset
str(bank.additional)
summary(bank.additional)
bank.additional <- subset(bank.additional[c(1,2,3,5,10,21)])
str(bank.additional)
colnames(bank.additional)
class(bank.additional)
barplot(table(bank.additional$y))

#convert data type of age to numberic
bank.additional$age <- as.numeric(bank.additional$age)
#Scales the "age" values to the range [0, 1], which is often 
#referred to as min-max scaling or normalization.
bank.additional$age <- (bank.additional$age-min(bank.additional$age))/(max(bank.additional$age)-min(bank.additional$age))

bank.additional$job <- as.numeric(bank.additional$job)
bank.additional$job <- (bank.additional$job-min(bank.additional$job))/(max(bank.additional$job)-min(bank.additional$job))

bank.additional$marital <- as.numeric(bank.additional$marital)
bank.additional$marital <- (bank.additional$marital-min(bank.additional$marital))/(max(bank.additional$marital)-min(bank.additional$marital))

bank.additional$default <- as.numeric(bank.additional$default)
bank.additional$default <- (bank.additional$default-min(bank.additional$default))/(max(bank.additional$default)-min(bank.additional$default))

bank.additional$day_of_week <- as.numeric(bank.additional$day_of_week)
bank.additional$day_of_week <- (bank.additional$day_of_week-min(bank.additional$day_of_week))/(max(bank.additional$day_of_week)-min(bank.additional$day_of_week))

bank.additional$y <- as.numeric(bank.additional$y)
bank.additional$y <- (bank.additional$y-min(bank.additional$y))/(max(bank.additional$y)-min(bank.additional$y))

str(bank.additional)
set.seed(1234)
ind <- sample(2, nrow(bank.additional), replace = TRUE, prob = c(0.8, 0.2))
train<-bank.additional[ind==1,]
test<-bank.additional[ind==2,]

library(neuralnet)
set.seed(3333)
#n <- neuralnet(y~age+job+marital+default+day_of_week, #dependent variable (y) is predicted based on the independent variables
 #              train, #The data frame train is the dataset used to train the neural network.
  #             hidden=3, # number of neurons in the hidden layer
   #            err.fct="sse", # error function used during training
    #           linear.output = FALSE, #neural network is being used for a non-linear regression problem
     #          rep=2, #training process is repeated twice.
      #         algorithm="rprop+", #training algorithm. "rprop+" stands for resilient back-propagation with weight backtracking
       #        lifesign = "full" #Controls the frequency and level of logging during the training process
        #       ) 

n <- neuralnet(y~age+job+marital+default+day_of_week,
               train, 
               hidden=3,
               err.fct="sse",
               linear.output = FALSE, 
               rep=2, 
               algorithm="rprop+", 
               lifesign = "full" 
) 

print(n)

plot(n, col.hidden = 'darkgreen', 
     col.hidden.synapse = "darkgreen",
     show.weights = T,
     information = T, 
     fill = "lightblue")

output <- compute(n, train[,-6])
head(output$net.result)
head(train[6,])

output <- compute(n, train[,-6])
p1 <- output$net.result
pred1 <- ifelse(p1>0.5,1,0)
tab1<- table(pred1, train$y)
tab1

sum(diag(tab1))/sum(tab1)
1 -sum(diag(tab1))/sum(tab1)

output <- compute(n, test[,-6])
p2 <- output$net.result
pred2 <- ifelse(p2>0.5,1,0)
tab2<- table(pred2, train$y)
tab2 

sum(diag(tab1))/sum(tab1)
1 -sum(diag(tab1))/sum(tab1)

