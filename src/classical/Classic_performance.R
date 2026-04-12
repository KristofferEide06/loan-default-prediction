# Logistical regression
library(class)


performance_logreg <- function(model, test, verbose = TRUE, threshold = 0.5) {
  # Predict probabilities
  prob <- predict(model, newdata = test, type = "response")
  
  model.pred <- ifelse(prob >= threshold, 1, 0)
  
  #Metrics
  
  actual <- as.numeric(as.character(test$loan_status))
  
  confTab.model <- table(Predicted = model.pred, actual = actual)
  
  misclass.model <- 1 - sum(diag(confTab.model))/sum(confTab.model)
  sensitivity.model <- confTab.model["1", "1"]/sum(confTab.model[, "1"])
  specificity.model <- confTab.model["0", "0"]/sum(confTab.model[, "0"])

  #ROC/PR and AUC
  predob <- ROCR::prediction(prob, actual)
  perf <- ROCR::performance(predob, "prec", "rec")
  auc.model <- ROCR::performance(predob, "auc")
  
  if (verbose) {
    cat("Misclassification rate: ", misclass.model, "\n") 
    cat("Sensitivity: ", sensitivity.model, "\n") 
    cat("Specificity : ", specificity.model, "\n") 
    cat("AUC: ", auc.model@y.values[[1]], "\n")
  }
  
  list(
    misclass_rate = misclass.model,
    sensitivity = sensitivity.model,
    specificity = specificity.model,
    auc = auc.model@y.values[[1]]
  )
}


# KNN
performance_knn <- function(k, train, test, verbose = TRUE) {
  # Converts factors to numerical values 
  train_x <- model.matrix(loan_status ~ . -1, data=train)
  test_x  <- model.matrix(loan_status ~ . -1, data=test)

  # Scale the sets
  center_vals <- colMeans(train_x)
  sd_vals <- apply(train_x, 2, sd)

  train_x <- (train_x - center_vals)/sd_vals
  test_x <- (test_x - center_vals)/sd_vals

  # Predict
  model.pred <- knn(train = train_x, test = test_x, cl = train$loan_status, k = k, prob = TRUE)
  
  prob <- ifelse(
    model.pred == "1",
    attr(model.pred, "prob"),
    1 - attr(model.pred, "prob")
  )
  
  #Metrics
  actual <- as.numeric(as.character(test$loan_status))
  
  confTab.model <- table(Predicted = model.pred, actual = actual)
  
  misclass.model <- 1 - sum(diag(confTab.model))/sum(confTab.model)
  sensitivity.model <- confTab.model["1", "1"]/sum(confTab.model[, "1"])
  specificity.model <- confTab.model["0", "0"]/sum(confTab.model[, "0"])
  
  #ROC/PR and AUC
  predob <- ROCR::prediction(prob, actual)
  perf <- ROCR::performance(predob, "prec", "rec")
  auc.model <- ROCR::performance(predob, "auc")
  
  if (verbose) {
    cat("Misclassification rate: ", misclass.model, "\n") 
    cat("Sensitivity: ", sensitivity.model, "\n") 
    cat("Specificity : ", specificity.model, "\n") 
    cat("AUC: ", auc.model@y.values[[1]], "\n")
  }
  
  list(
    misclass_rate = misclass.model,
    sensitivity = sensitivity.model,
    specificity = specificity.model,
    auc = auc.model@y.values[[1]]
  )
}
