library(here)
library(gbm)
library(ada)
library(lightgbm)
library(matrixStats)

source(here::here("data", "data.R"))
source(here::here("src", "trees", "Tree_analysis.R"))

#lgbm, with cv
cv.lgbm <- function(train, 
                    grid_num_leaves,
                    grid_learning_rate, 
                    grid_min_data_in_leaf, 
                    k = 5, seed = 123
) {
  results <- data.frame(
    num_leaves = integer(),
    min_data_in_leaf = integer(),
    learning_rate = numeric(),
    best_iter = integer(),
    best_metric = numeric()
  )
  
  x_train <- data.matrix(train[, setdiff(names(train), "loan_status")])
  y_train <-  as.numeric(as.character(train$loan_status))
  
  dtrain <- lgb.Dataset(data = x_train, label = y_train)
  
  best_score <- -Inf
  best_params <- NULL
  best_iter <- NULL
  
  set.seed(seed)
  
  for (num_leaves in grid_num_leaves) {
    for (learning_rate in grid_learning_rate) {
      for (min_data_in_leaf in grid_min_data_in_leaf) {
        params = list(
          objective = "binary",
          metric = "average_precision",
          is_unbalance = TRUE,
          num_leaves = num_leaves,
          min_data_in_leaf = min_data_in_leaf,
          learning_rate = learning_rate,
          feature_fraction = 0.8,
          bagging_fraction = 0.8,
          bagging_freq = 1L,
          feature_pre_filter = FALSE
        )
        model <- lgb.cv(
          params= params,
          data = dtrain,
          nrounds = 1000L,
          nfold = k,
          early_stopping_rounds = 50L,
          verbose = -1L
        )
        
        current_best_iter <- model$best_iter
        current_best_metric <- unlist(model$best_score)
        
        results <- rbind(
          results,
          data.frame(
            num_leaves = num_leaves,
            min_data_in_leaf = min_data_in_leaf,
            learning_rate = learning_rate,
            best_iter = current_best_iter,
            best_metric = current_best_metric
          )
        )
        
        if (current_best_metric > best_score) {
          best_score <- current_best_metric
          best_params <- params
          best_iter <- current_best_iter
          best_model <- model
        }
      }
    }
  }
  best_model <- lightgbm(
    data = dtrain,
    label = y_train,
    params = best_params,
    nrounds = best_iter,
    verbose = -1L
  )
  
  list(
    results = results,
    best_iter = best_iter,
    best_params = best_params,
    best_model = best_model
  )
}

x_test <- data.matrix(test[, setdiff(names(train), "loan_status")])
y_test <-  as.numeric(as.character(test$loan_status))

grid_num_leaves <- c(7L, 15L, 31L)
grid_min_data_in_leaf  <- c(10L, 25L, 50L)
grid_learning_rate <- c(0.01, 0.05, 0.1) #Suggestions from chatgpt


cv.lgbm.loan <- cv.lgbm(train = train,
                        grid_num_leaves = grid_num_leaves,
                        grid_learning_rate = grid_learning_rate,
                        grid_min_data_in_leaf = grid_min_data_in_leaf,
                        )

lgbm.loan <- cv.lgbm.loan$best_model
lgbm.loan.params <- cv.lgbm.loan$best_params
lgbm.loan.results <- cv.lgbm.loan$results

saveRDS(
  list(
    lgbm.params = lgbm.loan.params,
    lgbm.results = lgbm.loan.results
  ),
  here::here("models", "lgbm.rds")
)

lgb.save(lgbm.loan, here::here("models", "lgbm_model.txt"))

#bag lgbm
bag.lgbm <- function(train, test, response, params, iter, B = 100, seed = 123) {
  set.seed(seed)
  n <- nrow(train)
  models <- vector("list", B)
  probs <- matrix(NA, nrow = nrow(test), ncol = B)
  predictor_names <- setdiff(names(train), response)
  
  for (b in 1:B) {
    idx <- sample(seq_len(n), size = n, replace = TRUE)
    boot_train <- train[idx, ]
    x_boot <- data.matrix(boot_train[, predictor_names])
    y_boot <- as.numeric(as.character(boot_train[[response]]))
    dboot <- lgb.Dataset(data = x_boot, label = y_boot)
    
    model <- lightgbm(
      data = dboot,
      params = params,
      nrounds = iter,
      verbose = -1L
    )
    models[[b]] <- model
    prob <- predict(model, x_test)
    probs[, b] <- prob
  }
  prob_mean <- rowMeans(probs)
  prob_dev <- rowSds(probs)
  
  structure(
  list(
    models = models,
    prob = prob_mean,
    prob_dev = prob_dev
  ),
  class = "bag_lgbm"
  )
}

bag.lgbm.loans <- bag.lgbm(
  train = train,
  test = test,
  response = "loan_status",
  params = lgbm.loan.params,
  iter = cv.lgbm.loan$best_iter
)

pred.bag.lgbm.loans <- ifelse(bag.lgbm.loans$prob >= 0.5, 1, 0)

saveRDS(bag.lgbm.loans, here::here("models", "bag_lgbm.rds"))

#ada with cv
cv.ada <- function(train, grid_iter, grid_maxdepth, grid_nu, grid_type, grid_loss, k = 5, seed = 123) {
  results <- data.frame(
    iter = integer(),
    maxdepth = integer(),
    nu = numeric(),
    type = character(),
    loss = character(),
    mean_auc = numeric()
  )
  
  set.seed(seed)
  
  n <- nrow(train)
  fold_id <- sample(rep(1:k, length.out = n))
  
  idx = 1
  for (iter in grid_iter) {
    for (maxdepth in grid_maxdepth) {
      for (nu in grid_nu) {
        for (type in grid_type) {
          for (loss in grid_loss) {
            fold_auc <- c()
            
            for (fold in 1:k) {
              train_fold <- train[fold_id != fold, ]
              valid_fold <- train[fold_id == fold, ]
              
              model <- ada(
                loan_status ~ .,
                data = train_fold,
                iter = iter,
                nu = nu,
                type = type,
                loss = loss,
                control = rpart::rpart.control(maxdepth = maxdepth, cp = 0)
              )
              
              prob_matrix <- predict(model, newdata = valid_fold, type = "prob")  
              prob <- prob_matrix[, 2]
              
              actual <- valid_fold$loan_status
              
              predob <- ROCR::prediction(prob, actual)
              perf <- ROCR::performance(predob, "prec", "rec")
              
              auc <- perf@y.values[[1]]
              
              fold_auc <- c(fold_auc, auc)
            }
            results <- rbind(
              results,
              data.frame(
              iter = iter,
              maxdepth = maxdepth,
              nu = nu,
              type = type,
              loss = loss,
              mean_auc = mean(fold_auc)
              )
            )
            
            print(idx)
            idx = idx + 1
          }
        }
      }
    }
  }
  results <- results[order(-results$mean_auc), ]
  rownames(results) = NULL
  
  best <- results[1, ]
  
  final_model <- ada(
    loan_status ~ .,
    data = train,
    iter = best$iter,
    nu = best$nu,
    type = best$type,
    loss = best$loss,
    control = rpart::rpart.control(maxdepth = best$maxdepth, cp = 0)
  )
  
  list(
    results = results,
    final_model = final_model,
    best_params = best
  )
}

grid_iter = c(50L, 100L, 200L)
grid_maxdepth = c(1L, 2L, 3L)
grid_nu = c(0.1, 1)
grid_type = c("discrete", "real")
grid_loss = c("exponential", "logistic")

cv.ada.loan <- cv.ada(train = train, 
                      grid_iter = grid_iter,
                      grid_maxdepth = grid_maxdepth,
                      grid_nu = grid_nu,
                      grid_type = grid_type,
                      grid_loss = grid_loss)

ada.loan <- cv.ada.loan$final_model
ada.loan.params <- cv.ada.loan$best_params
ada.loan.results <- cv.ada.loan$results

saveRDS(
  list(
    ada.model = ada.loan,
    ada.params = ada.loan.params,
    ada.results = ada.loan.results
  ),
  here::here("models", "adaboost.rds")
)




