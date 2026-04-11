source(here::here("data", "data.R"))
source(here::here("src", "classical", "Classic_performance.R"))
# The tuning of df in cubic splines and k in KNN are in tuning.R

# cv logreg, cubic splines, testing for auc
cv.auc <- function(d1, d2, train, K = 5) {
  
  n <- nrow(train)
  folds <- sample(rep(1:K, length.out = n))
  
  aucs <- rep(NA, K)
  
  for (k in 1:K) {
    
    train_fold <- train[folds != k, ]
    val_fold   <- train[folds == k, ]
    
    model <- glm(
      loan_status ~ ns(loan_percent_income, df=d1) + ns(loan_int_rate, df=d2) + ., data = train_fold, family = "binomial")
    
    perf <- performance_logreg(model, val_fold, verbose=FALSE)
    aucs[k] <- perf$auc
  }
  
  mean(aucs)
}

# See from auc_mat matrix in tuning.R that df1=7 and df2=7 gives best auc from CV
d1 <- 7
d2 <- 7



# fitting of logreg with pca
logreg_pca <- function(train, test, var_percent=0.90) {
  # To convert factors into dummy variables
  x_train <- model.matrix(loan_status ~ . -1, data = train)
  x_test  <- model.matrix(loan_status ~ . -1, data = test)
  
  # Find principal components
  pca_comp <- prcomp(x_train, scale. = TRUE)
  
  pca_summary <- summary(pca_comp)
  
  cumul_var <- pca_summary$importance[3, ]  # cumulative variance
  n_comp <- which(cumul_var >= var_percent)[1] # choose the PC that explains 90% of variability
  
  # Transforms feature space into basis of principal components
  train_pca <- predict(pca_comp, newdata = x_train)[, 1:n_comp]
  test_pca  <- predict(pca_comp, newdata = x_test)[, 1:n_comp]
  
  train_pca_df <- data.frame(train_pca, loan_status = train$loan_status)
  test_pca_df  <- data.frame(test_pca,  loan_status = test$loan_status)
  
  logreg.pca <- glm(loan_status ~ ., data = train_pca_df, family = "binomial")

  
  list(
    model = logreg.pca,
    train_pca = train_pca_df,
    test_pca = test_pca_df,
    n_comp = n_comp,
    pca_comp = pca_comp
  )
  
}



# knn cv for tuning k

cv.knn.auc <- function(k, train, K = 5) {
  
  n <- nrow(train)
  folds <- sample(rep(1:K, length.out = n))
  
  aucs <- rep(NA, K)
  
  for (fold in 1:K) {
    
    train_fold <- train[folds != fold, ]
    val_fold   <- train[folds == fold, ]
    
    perf <- performance_knn(k, train_fold, val_fold, verbose = FALSE)
    
    aucs[fold] <- perf$auc
  }
  
  mean(aucs)
}

# finds k=30 is best from cv in tuning.R
best_k <- 30