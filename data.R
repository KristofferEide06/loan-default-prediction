df <- read.csv("loan_data.csv")

#Data analysis
names(df)
nrow(df)
ncol(df)

summary(df)
colSums(is.na(df))
sum(duplicated(df))

table(df$loan_status, useNA = "ifany")
prop.table(table(df$loan_status))


#Stratify the splits to avoid skewed sets with respect to loan_status
set.seed(123)

idx_0 <- which(df$loan_status == 0)
idx_1 <- which(df$loan_status == 1)

train_0 <- sample(idx_0, size = 0.8*length(idx_0))
train_1 <- sample(idx_1, size = 0.8*length(idx_1))

train_idx <- c(train_0, train_1)

train <- df[train_idx, ]
test <- df[-train_idx]