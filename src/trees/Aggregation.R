library(tree)
library(randomForest)
library(here)

source(here::here("data", "data.R"))

#Single tree
tree.loan <- tree(loan_status ~ ., data = train)

cv.loan <- cv.tree(tree.loan, FUN = prune.misclass)
cv.min.idx <- which.min(cv.loan$dev)
best.tree <- cv.loan$size[cv.min.idx]

prune.loan <- prune.misclass(tree.loan, best = best.tree)
#Bagging
set.seed(123)

bag.loan <- randomForest(loan_status ~ ., 
                         data = train,
                         mtry = ncol(train) - 1,
                         ntree = 500,
                         importance = TRUE
                         )

#Random forest
set.seed(123)

rf.loan <- randomForest(loan_status ~ ., 
                        data = train, 
                        mtry = as.integer(sqrt(ncol(train) - 1)),
                        ntree = 500,
                        importance = TRUE)

#Find ideal mtry
set.seed(123)

mtry.grid <- seq(ncol(train) - 1)
mtry.oob <- data.frame(
  mtry = integer(),
  oob.error = numeric()
)

for (m in mtry.grid) {
  cat("Running mtry =", m, "\n")
  rf.fit <- randomForest(loan_status ~ ., 
                         data = train, 
                         mtry = m, 
                         ntree = 500,
                         importance = TRUE)

  oob.err <- tail(rf.fit$err.rate[, "OOB"], 1)
  mtry.oob <- rbind(
    mtry.oob,
    data.frame(mtry = m, oob.error = oob.err)
  )
}

best.mtry.idx <- which.min(mtry.oob$oob.error)

best.mtry <- mtry.oob$mtry[best.mtry.idx]

set.seed(123)

rf.best.loan <- randomForest(loan_status ~ ., data = train, mtry = best.mtry, ntree = 500, importance = TRUE)

#Save
tree_models <- list(
  tree.loan = tree.loan,
  cv.loan = cv.loan,
  prune.loan = prune.loan,
  bag.loan = bag.loan,
  rf.loan = rf.loan,
  mtry.oob = mtry.oob,
  best.mtry = best.mtry,
  rf.best.loan = rf.best.loan
)

saveRDS(tree_models, here::here("models", "aggregation_models.rds"))

saveRDS(
  list(
    mtry.oob = mtry.oob,
    best.mtry.idx = best.mtry.idx,
    best.mtry = best.mtry
  ),
  here::here("models", "rf_tuning.rds")
)


