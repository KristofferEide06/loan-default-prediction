library(ROCR)
library(tree)
library(randomForest)

visualize_tree <- function(model, aggregation, show_summary = FALSE,
                           top_n = 10, tree_cex = 0.7, imp_cex = 0.8) {
  if (!aggregation) {
    if (show_summary) print(summary(model))
    op <- par(mar = c(1, 1, 2, 1))
    on.exit(par(op), add = TRUE)
    plot(model)
    text(model, pretty = 0, cex = tree_cex)
  } else {
    op <- par(mar = c(5, 14, 2, 1) + 0.1)
    on.exit(par(op), add = TRUE)
    varImpPlot(model, type = 1, cex = imp_cex)
  }
}

performance_tree <- function(model, test, verbose = FALSE) {
  model.pred <- predict(model, newdata = test, type = "class")
  
  #Metrics
  confTab.model <- table(Predicted = model.pred, actual = test$loan_status)
  
  if (verbose) {
    cat("Confusion matrix: \n")
    print(confTab.model)
  }
  
  misclass.model <- 1 - sum(diag(confTab.model))/sum(confTab.model)
  sensitivity.model <- confTab.model["1", "1"]/sum(confTab.model[, "1"])
  specificity.model <- confTab.model["0", "0"]/sum(confTab.model[, "0"]) #Consider adding positive class improvement
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
  
  if (verbose) {
  plot(perf)
  }
  
  auc.model <- ROCR::performance(predob, "auc")
  cat("AUC: ", auc.model@y.values[[1]], "\n")
  
  list(
    misclass_rate = misclass.model,
    sensitivity = sensitivity.model,
    specificity = specificity.model,
    auc = auc.model@y.values[[1]]
  )
}

performance_boost_tree <- function(model, test, verbose = FALSE, threshold = 0.5, n.trees = NULL) { #Copilot used to help with this one(prob >= threshold, 1, 0)
  if (inherits(model, "lgb.Booster")) {
    x_test <- data.matrix(test[, setdiff(names(test), "loan_status")])
    prob <- predict(model, x_test)
    model.pred <- ifelse(prob >= threshold, 1, 0)
    
  } else if(inherits(model, "ada")) {
    pred_class <- predict(model, newdata = test, type = "vector")
    prob_mat <- predict(model, newdata = test, type = "prob")
    prob <- prob_mat[, 2]
    model.pred <- as.numeric(as.character(pred_class))
  } else if(inherits(model, "bag_lgbm")) {
    prob <- model$prob
    model.pred <- ifelse(model$prob >= 0.5, 1, 0)
  }
  
  #Misclass and conftab
  actual = as.numeric(as.character(test$loan_status))
  
  confTab.model <- table(Predicted = model.pred, actual = actual)
  
  if (verbose) {
  cat("Confusion matrix: \n")
  print(confTab.model)
  }
  
  misclass.model <- 1 - sum(diag(confTab.model))/sum(confTab.model)
  sensitivity.model <- confTab.model["1", "1"]/sum(confTab.model[, "1"])
  specificity.model <- confTab.model["0", "0"]/sum(confTab.model[, "0"])
  
  cat("Misclassification rate: ", misclass.model, "\n") 
  cat("Sensitivity: ", sensitivity.model, "\n") 
  cat("Specificity : ", specificity.model, "\n") 
  
  #ROC/PR and AUC
  predob <- ROCR::prediction(prob, actual)
  perf <- ROCR::performance(predob, "prec", "rec")
  
  if (verbose) {
  plot(perf)
  }
  
  auc.model <- ROCR::performance(predob, "auc")
  cat("AUC: ", auc.model@y.values[[1]], "\n")
  
  list(
    misclass_rate = misclass.model,
    sensitivity = sensitivity.model,
    specificity = specificity.model,
    auc = auc.model@y.values[[1]]
  )
}
