Loan <- read.csv(here::here("data", "Loan_data.csv"))

#Data analysis
head(Loan)
summary(Loan)
names(Loan)

nrow(Loan)
ncol(Loan)

colSums(is.na(Loan))
sum(duplicated(Loan))

table(Loan$loan_status, useNA = "ifany")
prop.table(table(Loan$loan_status))

#Clean data
str(Loan)

Loan$loan_status <- factor(Loan$loan_status)
Loan$person_gender <- factor(tolower(trimws(Loan$person_gender)))
Loan$person_education <- factor(tolower(trimws(Loan$person_education)))
Loan$person_home_ownership <- factor(toupper(trimws(Loan$person_education)))
Loan$loan_intent <- factor(toupper(trimws(Loan$loan_intent)))
Loan$previous_loan_defaults_on_file <- factor(tolower(trimws(Loan$previous_loan_defaults_on_file)), levels = c("no", "yes"))

Loan
#Stratify the splits to avoid skewed sets with respect to Loan_status
set.seed(123)

idx_0 <- which(Loan$loan_status == 0)
idx_1 <- which(Loan$loan_status == 1)

train_0 <- sample(idx_0, size = 0.8*length(idx_0))
train_1 <- sample(idx_1, size = 0.8*length(idx_1))

train_idx <- c(train_0, train_1)

train <- Loan[train_idx, ]
test <- Loan[-train_idx, ]