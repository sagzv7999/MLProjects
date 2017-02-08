library(caret)
library(mlbench)
dat=read.table("logit.txt",header=FALSE)
names(dat)
folds <- createFolds(dat$V5,10)
train_data=as.matrix(folds[1:9])
test_data=as.matrix(folds[10])
mydat= dat[sample(nrow(dat)),]
split=0.9
rainIndex <- createDataPartition(mydat$V1, p=split, list=FALSE)
data_train <- mydat[ rainIndex,]
data_test <- mydat[-rainIndex,]
data_train<- cbind(rep(1,nrow(mydat)),mydat)

data_test_labels <- as.matrix(data_test[,5])
data_train_lables <- as.matrix(data_train[,6])
data_test <- data_test[,-5]
data_train<-data_train[,-6]

data_train =as.matrix(data_train)
Y= data_train_lables


sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}

cost <- function(theta)
{
  m <- nrow(data_train)
  for(i in 1:m){
  g <- sigmoid(z)
  J <- (1/m)*sum((-Y*log(g)) - ((1-Y)*log(1-g)))
  return(J) 
  }
}

iterations<-1
j<-vector()
alpha<-0.001
theta<-c(0,0)



#My loop

while(iterations<=10){
  
  coste<-c(0,0)
  suma<-0
  
  for(i in 1:m){
    
    # h<-1/(1+exp(-Q*x)
    
    h<-1/(1+exp(-z))
    
    #Cost(hQ(x),y)=y(i)*log(hQ(x))+(1-y(i))*log(1-hQ(x))
    
    cost<-((Y%*%log(h))+((1-Y)%*%log(1-h)))
    
    #sum(cost) i=1 to m
    
    suma<-suma + cost;
    
    #Diferences=(hQ(x(i))-y(i))*x(i)
    
    difference<-(h-Y)*data_train[i,]  
    
    #sum the differences 
    
    coste<-coste+difference
    
    #calculation thetas and upgrade = Qj:= Qj - alpha* sum((h-y[i])*x[i,]*x(i))
    
    theta[1]<-(theta[1]-alpha*1/m*(coste[1]))
    theta[2]<-(theta[2]-alpha*1/m*(coste[2]))
    
  }
  #J(Q)=(-1/m)* sum ( y(i)*log(hQ(x))+(1-y(i))*log(1-hQ(x)))
  
  j[iterations]<-(-1/m)*suma
  
  iterations=iterations+1
  
}







