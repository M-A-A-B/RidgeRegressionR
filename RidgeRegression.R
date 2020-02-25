library(e1071)
library(caret)

regressionCoefficients <- function(X,XLabels,lambda) {
  X = cbind(c(1),X)
  return(solve(t(X)%*%X+lambda*diag(ncol(X)))%*%t(X)%*%XLabels)
}

predictions <- function(testX,regressionCoefficients) {
  return(sum(regressionCoefficients[-1,] * testX) + regressionCoefficients[1,1])
}

lambdaIteration <- function(lambda,trainX,trainXLabels,testX,testXLabels) {
  print("#######################################################################################")
  print('For lambda:')
  print(lambda)
  
  regressionCoeff = regressionCoefficients(trainX,trainXLabels,lambda)
  
  predtrainX <- apply(trainX,1,predictions,regressionCoeff)
  predtestX <- apply(testX,1,predictions,regressionCoeff)
  
  print(predtestX[1])
  print(predtestX[10])
  print(predtestX[15])
  print(predtestX[100])
  print(predtestX[120])
  print(predtestX[200])
  
  mapPredtrainX <- ifelse(predtrainX>=mean(predtrainX), 5, 2)
  mapPredtestX <- ifelse(predtestX>=mean(predtestX), 5, 2)

  print("RESULTS FOR TRAINING DATA")
  print(confusionMatrix(factor(as.matrix(mapPredtrainX)),factor(trainXLabels)))
  print("RESULTS FOR TEST DATA")
  print(confusionMatrix(factor(as.matrix(mapPredtestX)),factor(testXLabels)))
  
}

driver <- function() {
  trainX = as.matrix(read.table('train2_5.txt'))
  trainXLabels = as.matrix(read.table('train2_5Labels.txt'))
  testX = as.matrix(read.table('test2_5.txt'))
  testXLabels = as.matrix(read.table('test2_5Labels.txt'))
  
  r = trainX[1,]   #first digit image, i.e., image in row 1
  im = matrix(r,nrow=16,byrow=TRUE)   #convert vector to image
  image(im[,ncol(im):1])    #view image

 lambda = as.matrix(c(0.001,1,1000,25))
  apply(lambda,1,lambdaIteration,trainX,trainXLabels,testX,testXLabels)

}

driver()
