---
title: "Reproducing Results from Above Market Value: Predictive Insights for Conneticut Real Estate Decisions"
author: "Ben Jackals and Benjamin Weitzner"
date: "12/10/2025"
output: html_document
---

## Introduction
This repository contains the code and data required to reproduce the results found in "Predictive Insights for Conneticut Real Estate Decisions". Specifically, to run create models that can describe and predict market value for within the Conneticut real estate market. 

## Requirements
To install the required R packages, run the following code in R:


```r
install.packages(c(
  "dplyr", "tidyverse", "zoo", "fredr", "forcats", "glmnet",
  "pROC", "ggplot2", "Matrix", "tidymodels", "randomForest",
  "rpart", "logistf", "ranger"
))

```

## Data

We use 2 sources of data containing (1) Real Estate data and (9) Macroeconomic metrics:


```r
list.files("Real_Estate_Sales_2001-2023_GL_20251112.csv")
seriesids("UNRATE", "MVMTD027MNFRBDAL","REITTMA","GDPC1","NASDAQCOM","NASDAQNQEM","MEDCPIM158SFRBCLE","PCE","GDP")
```

## Reproduce
1. Run `CleaningRE.R` to merge all of the data and clean it.
2. Run `GLMRE.R` To create the GLM models, the final GLM model (m9), and associated graphs.
3. Run `RandomForestRE.R` To create the Final Forests and associated graphs. This will create the VI graph, which the GLM variable inputs are based upon.
4. Run `Lasso&RidgeRE.R` To create the penalized lasso and ridge models and associated graphs.
5. Run `ExtraGraphsRE.R` To create the result focused graphs and extra confusion matrices.

## References
ChatGPT: Helped debug the code, specifically anything involving ranger in the RandomForestRE.R.
