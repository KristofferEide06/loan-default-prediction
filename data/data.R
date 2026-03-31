Loan <- read.csv(here::here("data", "Loan_data.csv"))

#Data analysis
loan.na.loan_status <- Loan[is.na(Loan$loan_status), ]
Loan <- Loan[!is.na(Loan$loan_status), ]

#---Transform predictors---
Loan$loan_status <- factor(Loan$loan_status, levels = c(0, 1))
Loan$person_gender <- factor(tolower(trimws(Loan$person_gender)))
Loan$person_education <- factor(tolower(trimws(Loan$person_education)))
Loan$person_home_ownership <- factor(toupper(trimws(Loan$person_home_ownership)))
Loan$loan_intent <- factor(toupper(trimws(Loan$loan_intent)))
Loan$previous_loan_defaults_on_file <- factor(tolower(trimws(Loan$previous_loan_defaults_on_file)), levels = c("no", "yes"))

#---Outliers---
rows.outliers <- which(
  Loan$person_age < 18 |
  Loan$person_age > 100 |
  Loan$person_income < 0 |
  Loan$loan_amnt <= 0 |
  Loan$loan_int_rate <= 0 |
  Loan$loan_percent_income < 0 |
  Loan$person_emp_exp < 0 |
  Loan$person_emp_exp > Loan$person_age |
  Loan$cb_person_cred_hist_length < 0 |
  Loan$cb_person_cred_hist_length > Loan$person_age
)

Loan.outliers <- Loan[rows.outliers, ]
Loan <- Loan[-rows.outliers, ]


#---New predictors---
Loan$emp_exp_to_age <- Loan$person_emp_exp/Loan$person_age
Loan$cred_hist_to_age <- Loan$cb_person_cred_hist_length/Loan$person_age

#Splits, stratify to avoid skewed sets
set.seed(123)

idx_0 <- which(Loan$loan_status == "0")
idx_1 <- which(Loan$loan_status == "1")

train_0 <- sample(idx_0, size = floor(0.8*length(idx_0)))
train_1 <- sample(idx_1, size = floor(0.8*length(idx_1)))

train_idx <- c(train_0, train_1)

train <- Loan[train_idx, ]
test <- Loan[-train_idx, ]
