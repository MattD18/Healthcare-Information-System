## CROSS VALIDATION

#This function performs k-fold cross validation of a linear model on the 
#training data using mean square error

#k is a integer determining number of folds
#train is the data set model is cross validated on
#formula of the form Y~ X1 + X2 + ... + Xn is linear model being tested
k_cross_validation <- function(k,train,formula){
  #sets partition size for each iteration
  partition <- round(nrow(train) / k)
  #vector to store error
  errors <- vector(length = k)
  
  for (i in 1:k) {
    #creates training and test sets for this iteration
    s <- (i-1)*partition + 1 
    if (i == k) {
      f <- nrow(train)
    } else{
      f <- i*partition
    }
    fold_test <- train[s:f,]
    fold_train <- train[-(s:f),]
    
    #fits linear model
    model <- lm(formula,fold_train)
    
    #stores out of sample error 
    errors[i] <- MSE(model,fold_test,all.vars(formula)[1])
  }
  
  kcv_error <- mean(errors)
  return(kcv_error)
  
}



## HELPER FUNCTIONS

#returns MSE of predictions from given model
MSE <- function(model, data, target_var){
  predicted <- predict(model, data)
  observed <- data[target_var]
  error <- sum((predicted - observed)^2) / length(predicted)
  return(error)
}


