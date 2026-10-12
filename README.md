# Loan Default Prediction

Statistical learning project for predicting whether a borrower will default based on financial and demographic data.

We compare classical and tree-based classification methods, including logistic regression, KNN, random forest, AdaBoost and LightGBM. Model selection and tuning use cross-validation, with performance evaluated using classification metrics and AUC. In the final comparison, the tree-based models performed best, with random forest giving the strongest overall performance.

The full analysis is available in [`report/Report.pdf`](report/Report.pdf), with implementations under `src/classical/` and `src/trees/`.

Data: [Bank Loan Data on Kaggle](https://www.kaggle.com/datasets/udaymalviya/bank-loan-data/data)

## Authors

- Kristoffer Eide
- Michael Birkeland

TMA4268 – Statistical Learning, Spring 2026
