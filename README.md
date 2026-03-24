# Multivariate-analysis-with-R
Multivariate statistical analysis in R: PCA, MCA, CA for dimensionality reduction and pattern exploration, with linear regression for predictive modeling.

 
---
 
## 📌 Methods Covered
 
- **Principal Component Analysis (PCA / ACP)** — reduces dimensionality of continuous data (student grades), visualizes clustering by specialization
- **Multiple Correspondence Analysis (MCA / AFCM)** — explores associations between categorical health indicators (diabetes dataset)
- **Correspondence Analysis (CA / AFC)** — studies the relationship between age groups and Polyuria via contingency table + chi-square test
- **Linear Regression (LR)** — models `crim` (crime rate) as a function of `medv` (simple) and all variables (multiple) using the Boston Housing dataset
 
---
---
 
## 📁 Structure
 
| Method | Dataset |
|---|---|---|
| `PCA/` | Student grades |
| `CA_MCA/` | Diabetes early-stage prediction |
| Simple & Multiple LR | Boston Housing |
 
---
 
## 🛠️ Packages
 
```r
install.packages(c("mlbench", "FactoMineR", "factoextra", "ggplot2", "dplyr", "readxl", "readr", "gridExtra"))
```
 
---
 
