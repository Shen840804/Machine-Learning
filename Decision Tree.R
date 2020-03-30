#Decision Tree

#Iris dataset
library('rpart')
library('rpart.plot')

data("iris")
s<-sample(150,100)
iris_train=iris[s,]
iris_test=iris[-s,]

dtm=rpart(formula = Species~.,
          iris_train,
          method = "class")
dtm

pred_result=predict(dtm,iris_test,type = 'class')

table(iris_test[,5],pred_result)


accuracy=sum(pred_result==iris_test$Species)/length(iris_test[,1])









############################################


library(caTools)
liibrary(rpart)
library(rpart.plot)

data("iris")
trainIndex=sample.split(iris$Species,2/3) #Take two-thirds of the data set as training data
iris.train=iris[trainIndex,]
iris.test=iris[!trainIndex,]

dtm=rpart(formula = Species~.,
          iris.train,
          method = "class")
dtm

pred_result=predict(dtm,iris.test,type = 'class')

table(iris.test[,5],pred_result)


accuracy=sum(pred_result==iris.test$Species)/length(iris.test[,1])

##########################################################################

library(caTools)
library(rpart)
library(rpart.plot)

mfile <- "C:/Users/Shen/Desktop/研究所課程/Machine Learning/Data/mushrooms.csv"   # specifies the location of data file

# read the data from day1survey.csv into a table named mushrooms
mushrooms<- read.table(mfile,
                       sep=",",
                       header=TRUE,
                       as.is=TRUE)
dim(mushrooms)
head(mushrooms)

trainIndex=sample.split(mushrooms$class,2/3) #Take two-thirds of the data set as training data
m.train=mushrooms[trainIndex,]
m.test=mushrooms[!trainIndex,]

dtm=rpart(class~.,
          m.train,
          method  = 'class')

pred=predict(rfm,m.test,type = 'class')

table(m.test$class,pred)

mean(m.test$class==pred) #accuracy

#sum(pred==m.test$class)/length(m.test[,1]) 

rpart.plot(dtm,type = 4,extra = 1,
           clip.right.labs = F,
           branch=1,under = T,box.palette = "RdYlGn"
)
rpart.rules(dtm,cover = T)
