
library('kernlab')

iris2=iris[51:150,3:5]

result=ksvm(Species~.,data=iris2,
            kernel='rbfdot',
            kpar=list(sigma=0.5),
            scale=FALSE)
windows()
plot(result,data=iris2)

####################################

X=as.matrix(iris[,-5])
label=as.numeric(iris[,5])

Jvalue=function(X,label,sigmavalue){
  
  rbf=rbfdot(sigma=sigmavalue)
  K=kernelMatrix(rbf,X)
  
  withinSum=0
  num=matrix(0,1,max(label))
  for (i in 1:max(label)){
    indexI=label==i
    withinSum=withinSum+sum(K[indexI,indexI])
    num[i]=sum(indexI)
  }
  
  betweenSum=sum(K)-withinSum
  w=withinSum/sum(num^2)
  b=betweenSum/((sum(num))^2-sum(num^2))
  J=1-w+b
}

sigma=seq(0,2,length=100)
J=matrix(0,1,length(sigmavalue))
for (i in 1:length(J)){
  J[i]=Jvalue(X=X,label=label,sigmavalue=sigma[i])
}

window()
plot(sigma,J,type='l')

opt=optimize(Jvalue,c(0,1000),X=X,label=label)
optSigma=opt$minimum
optSigma
