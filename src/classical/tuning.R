source(here::here("src", "classical", "Classic_model.R"))

# Cubic splines tuning

# find optimal df1 and df2 (degrees of freedom for the cubic splines)
df1_vals <- c(4, 5, 6, 7, 8)
df2_vals <- c(4, 5, 6, 7, 8)
auc_mat <- matrix(NA, length(df1_vals), length(df2_vals))
for (i in seq_along(df1_vals)) {
  for (j in seq_along(df2_vals)) {
    
    d1 <- df1_vals[i]
    d2 <- df2_vals[j]
    
    auc_mat[i, j] <- cv.auc(d1, d2, train)
    
    iter <- (i - 1) * length(df2_vals) + j
    cat("Iteration", iter, "/", length(df1_vals) * length(df2_vals), "\n")
  }
}

# auc_mat
# idx <- which(auc_mat == max(auc_mat), arr.ind = TRUE)
# d1 <- df1_vals[idx[1]]
# d2 <- df2_vals[idx[2]]
# cat("Best df for loan_percent_income:", df1_vals[idx[1]], "\n")
# cat("Best df for loan_int_rate:", df2_vals[idx[2]], "\n")


# KNN tuning


k_vals <- 1:30
auc_vec <- rep(NA, length(k_vals))

for (i in seq_along(k_vals)) {
  
  k <- k_vals[i]
  auc_vec[i] <- cv.knn.auc(k, train)
  
  cat("Iteration", i, "/", length(k_vals), "\n")
}

# auc_vec
# 
# idx <- which.max(auc_vec)
# best_k <- k_vals[idx]
# 
# cat("Best k:", best_k, "\n")
# cat("Best AUC:", auc_vec[idx], "\n")
