#Logistic Regression for Two classes with R

data(mtcars)

library('caTools')
trainIndex=sample.split(mtcars$vs,2/3) #Take two-thirds of the data set as training data
mtcars.train=mtcars[trainIndex,]
mtcars.test=mtcars[!trainIndex,]     #Take the rest of the data as testing data

model=glm(formula = vs ~ wt + disp,
          family = "binomial",
          data=mtcars.train)
summary(model)   
# the lower AIC value, the better
# the model with the lowest AIC value should be chosen(the best fitting model)

pred_result=round(predict(model,mtcars.test,type = 'response'))

accuracy=(sum(pred_result==mtcars.test$vs)/length(mtcars.test[,1]))*100
