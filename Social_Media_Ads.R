#datset = social media ads
#algorithm = svm

d1=read.csv(file.choose())
View(d1)
#eliminating unnecessary variables
d1=d1[3:5]
#or d1=d1[c(-1,-2)]
#or d1=d1[c(3,4,5)]
#checking for missing values
sum(is.na(d1))
#check if dependent variable is categorical or not
table(d1$Purchased)
##Classification model need to  be built
class(d1$Purchased)
#Encode the Dependent variable into factor
d1$Purchased=factor(d1$Purchased,levels=c(0,1))
class(d1$Purchased)
#scaling-- standarization or normalization
d1[-3]=scale(d1[-3])
#Model building
#split dataset
library(caTools)
split=sample.split(d1$Purchased,SplitRatio = 0.75)
t1=subset(d1,split==T)
t2=subset(d1,split==F)
#KNN
library(class)
c1=knn(t1,t2,k=36,cl=t1$Purchased)
library(caret)
confusionMatrix(c1,t2$Purchased)                      #96

#Naive Bayes
library(e1071)
c2=naiveBayes(x=t1[-3],y=t1[3])
y2=predict(c2,t2[-3])
library(caret)
confusionMatrix(y2,t2$Purchased)                      #94

# Decision tree
library(rpart)
c3 <-rpart(formula = Purchased ~.,data = t1)
y3 <- predict(c3,t1[,-3],type = 'class')
confusionMatrix(y3,t1[,3])                            #91

# Random Forest
library(randomForest)
c4 <- randomForest(t1[,-3],t1[,3],ntree = 500)
y4<-predict(c4,t2[,-3])
confusionMatrix(y4,t2[,3])                           #93


