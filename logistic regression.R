#Logistic Regression for Two classes with R

iris

names(iris)  #show all the variables

index=iris$Species != 'setosa' 
iris2=iris[index,]         
levels(iris2$Species)
iris2$Species=factor(iris2$Species)  #remove 'setosa' from levels
levels(iris2$Species)

library('caTools')
trainIndex=sample.split(iris2$Species,2/3) #Take two-thirds of the data set as training data
iris2.train=iris2[trainIndex,]
iris2.test=iris2[!trainIndex,]     #Take the rest of the data as testing data

result=glm(Species~Petal.Length, 
           data=iris2.train,
           family = 'binomial')  
summary(result)

pred_result=round(predict(result,iris2.test,type = 'response'))
pred_result=factor(pred_result,levels=c(0,1),
                   labels=c('versicolor','virginica'))

accuracy=(sum(pred_result==iris2.test$Species)/length(iris2.test[,1]))*100

print("Accuracy : " accuracy )



###########################################################################

#Logistic Regression for Multiclasses with R

library('mlogit')

iris1=mlogit.data(iris,choice = 'Species',shape ='wide',
                  drop.index = T) 

result=mlogit(Species~0|Petal.Length,
              data = iris1,
              reflevel = 'virginica')
summary(result)

newPetal.Length=c(1,4.35,6.9)

for(i in 1:length(result$coefficients)){
  assign(sprintf("beta%d",i),result$coefficients[i])
}
 
 de=1+exp(beta1+beta3*newPetal.Length)+exp(beta2+beta4*newPetal.Length)

 p1=exp(beta1+beta3*newPetal.Length)/de
 p2=exp(beta2+beta4*newPetal.Length)/de
 p3=1/de

 p1=round(p1,3)
 p2=round(p2,3) 
 p3=round(p3,3)
 
 ############################################################
 
 #Multiple Logistic Regression for Multiclasses with R
 
 library('mlogit')
 
 iris1=mlogit.data(iris,choice = 'Species',shape ='wide',
                   drop.index = T) 
 
 result=mlogit(Species~0|Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
               data = iris1,
               reflevel = 'virginica')
 summary(result)
 
 #############################################################
 
 #Mulyiple Logistic Regression for Two classes with R
 
 names(iris)  #show all the variables
 
 index=iris$Species != 'setosa' 
 iris2=iris[index,]         
 levels(iris2$Species)
 iris2$Species=factor(iris2$Species)  #remove 'setosa' from levels
 levels(iris2$Species)
 
 library('caTools')
 trainIndex=sample.split(iris2$Species,2/3) #Take two-thirds of the data set as training data
 iris2.train=iris2[trainIndex,]
 iris2.test=iris2[!trainIndex,]     #Take the rest of the data as testing data
 
 result=glm(Species~Sepal.Length+Sepal.Width+Petal.Length, 
            data=iris2.train,
            family = 'binomial')  
 summary(result)
 
 pred_result=round(predict(result,iris2.test,type = 'response'))
 pred_result=factor(pred_result,levels=c(0,1),
                    labels=c('versicolor','virginica'))
 
 accuracy=(sum(pred_result==iris2.test$Species)/length(iris2.test[,1]))*100
 