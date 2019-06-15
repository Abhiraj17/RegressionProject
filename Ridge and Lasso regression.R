library(ISLR)
data("Hitters")
?Hitters
str(Hitters)#gives structure of data set
Hitters2<-data.frame(Hitters)
Hitters2<-na.omit(Hitters2)
head(Hitters2)

#1. grid of lamda values
#2.for each lamda find B0 to Bp
#3. for each set of lamda and B find error
#4. plot errors
#5. find best min error
#6. find b corresponding to best
#7.use for prediction

#notes ususally we find best at low lamda 

#create sequence of small values and take power of those
#so initially values will be small and then goes on increasing

#as lamda increses B values shrinks
#only B0 wont shrink

x<-model.matrix(Salary~.,data=Hitters2)
head(x)

head(Hitters2)
contrasts(Hitters2$League)
contrasts(Hitters2)
y<-Hitters2$Salary
head(y)
grid=
seq(10,-2,length=100)
head(grid)
grid[33]
grid[91:100]
grid<-10^seq(10,-2,length=100)
library(glmnet)
ridge.model<-glmnet(x,y,alpha=0,lambda = grid)

dim(coef(ridge.model))
#first dim No of B, Second No of Lambda
#remove B0, B0 never shrinks, this is not part of L2norm and L1norm
#find error always with B1 , B2 to Bp

head(coef(ridge.model),2)

x<-x[,-1]
head(x)


ridge.model<-glmnet(x,y,alpha=0,lambda = grid)
ridge.model$lambda[c(50,60,70,80,100)]
coef(ridge.model)[-1,c(50,60,70,80,100)]

#overall B value, dont take B0
sqrt(sum((coef(ridge.model)[-1,50]^2)))

#prepare training and test data
?set.seed
?sample
head(x)
#to repeat result means to take same random sampling each time
set.seed(1)
train<-sample(1:nrow(x),nrow(x)*0.75)
test<--train
y.test<-y[test]
head(y.test)
x[train]
ridge.model=glmnet(x[train,],y[train],alpha = 0,lambda=grid)
ridge.predict<-predict(ridge.model,s=bestlam,newx=x[test,]) #s is lambda, alpha = 0 for ridge, 1=lasso
error<-mean((y.test-ridge.predict)^2)
error
#0 105699
#4      42661
#50     81237
#100    79230
#500    78325
#1000   82362
#10000  129401
#100000 161914
#bestlam 94822
#plot graph
#cross validation
set.seed(1)
train<-sample(1:nrow(x),nrow(x)*0.75)
test<--train
y.test<-y[test]
cv.out<-cv.glmnet(x[train,],y[train],alpha=0,lambda=grid)
plot(cv.out)
?plot
bestlam<-cv.out$lambda.min
bestlam

#Lasso regression, alpha=1
set.seed(1)
train<-sample(1:nrow(x),nrow(x)*0.75)
test<--train
y.test<-y[test]
head(y.test)
x[train]
lasso.model=glmnet(x[train,],y[train],alpha = 1,lambda=grid)
lasso.predict<-predict(lasso.model,s=lassobestlam,newx=x[test,]) #s is lambda, alpha = 0 for ridge, 1=lasso
error<-mean((y.test-lasso.predict)^2)
error
#0 105699
#4      42661
#50     81237
#100    79230
#500    78325
#1000   82362
#10000  129401
#100000 161914
#bestlam 94822
#plot graph
#cross validation
set.seed(1)
train<-sample(1:nrow(x),nrow(x)*0.75)
test<--train
y.test<-y[test]
lassocv.out<-cv.glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lassocv.out)
?plot
lassobestlam<-lassocv.out$lambda.min
lassobestlam


?cv.glmnet

lasso.model$lambda[c(50,60,70,80,100)]
coef(lasso.model)[-1,c(50,60,70,80,100)]
coef(ridge.model)[-1,c(50,60,70,80,100)]

#lasso have most of the coeff as 0
#so lasso is simple as compared to ridge
