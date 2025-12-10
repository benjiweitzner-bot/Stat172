Stat172
---
title: "Reproducing Results from Above Market Value: "Predictive Insights for Conneticut Real Estate Decisions"
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

We use 12 sources of data containing Real Estate data and Macroeconomic metrics:


```r
list.files("Real_Estate_Sales_2001-2023_GL_20251112.csv")
seriesids("UNRATE", "MVMTD027MNFRBDAL","REITTMA","GDPC1","NASDAQCOM","NASDAQNQEM","MEDCPIM158SFRBCLE","PCE","GDP")
```
