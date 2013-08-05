#Find KNN classifier with n-fold cross-validation 

#PREPROCESS
library(DMwR)
library(class)
library(ggplot2)
irisData<-iris

#Remove species names, store as labels
labels<-irisData$Species
irisData$Species<-NULL

set.seed(1)

#Define n-fold cross-validation function
f1<-function(max.k,max.folds,train.pct) {

#Initialize dataframes and matrix to hold results
this.err<-data.frame()
k.errors<-data.frame()
err.matrix <- matrix(data=NA,nrow=max.folds,ncol=max.k)
#max.k<-10 #Define max k
#max.folds<-5 #Define folds
#train.pct<-0.8 #Define train percentage

#Define iteration loops for k nearest neighbors and n cross-validation folds
for(k in 1:max.k) {  
  for(n in 1:max.folds) {
    
    #Split data into train and test sets for nth fold
    N<-nrow(irisData)
    train.index<-sample(1:N, train.pct*N)
    
    train.data<-irisData[train.index,]
    test.data<-irisData[-train.index,]
    
    train.labels<-as.factor(as.matrix(labels)[train.index,])
    test.labels<-as.factor(as.matrix(labels)[-train.index,])
    
    #knn.fit is being overwritten at every step
    #we want to create a separate variable for each model run
    #and then at prediction, take the linear average of all models
    
    knn.fit<-knn(train=train.data,
                 test=test.data,
                 cl=train.labels,
                 k=k
    )
    
    #Compile and print errors from knn.fit for each fold
    err.matrix[n,k] <- sum(test.labels !=knn.fit)/ length(test.labels)
    print(paste('Fold=',n, ', k=', k, ', Error=', err.matrix[n,k]))
    #Find the mean of errors from each fold
    k.errors <- colMeans(err.matrix)
    }
  } 
return(k.errors) 
 }          

#Run function for selected values of k, n, and train.pct
knnrun.errors <- f1(10,5,0.8)
#Plot errors, average for each k
results.plot<-plot((1:length(knnrun.errors)),knnrun.errors, xlab="k",ylab="Mean Errors",main='KNN Results from Cross-Validation',col.main="red",ylim=c(0.0,0.1))

