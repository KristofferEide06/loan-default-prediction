library(ROCR)
library(tree)
library(randomForest)

visualize_tree <- function(model, aggregation) {
  if (!aggregation) {
    print(summary(model))
    plot(model)
    text(model, pretty = 0)
  } else {
    print(importance(model))
    varImpPlot(model)
  }
}

performance_tree <- function(model, test) {
  model.pred <- predict(model, newdata = test, type = "class")
  
  #Metrics
  confTab.model <- table(Predicted = model.pred, actual = test$loan_status)
  print(confTab.model)
  
  
  misclass.model <- 1 - sum(diag(confTab.model))/sum(confTab.model)
  sensitivity.model <- confTab.model[2, 2]/sum(confTab.model[2, ])
  specificity.model <- confTab.model[1, 1]/sum(confTab.model[1, ]) #Consider adding positive class improvement
  cat("Misclassification rate: ", misclass.model, "\n") 
  cat("Sensitivity: ", sensitivity.model, "\n") 
  cat("Specificity : ", specificity.model, "\n") 
  
  #ROC/PR and AUC
  if (inherits(model, "tree")) {
    prob <- predict(model, newdata = test)
  } else {
    prob <- predict(model, newdata = test, type = "prob")
  }
  pred <- prob[, "1"]
  
  predob <- ROCR::prediction(pred, test$loan_status)
  perf <- ROCR::performance(predob, "prec", "rec") #Change to tpr, fpr for ROC. PR generally better for rare classes
  
  plot(perf)
  
  auc.model <- ROCR::performance(predob, "auc")
  cat("AUC: ", auc.model@y.values[[1]], "\n")
}
