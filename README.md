# Fairness

## Description

This package contains functions to compute fairness measures of classifiers such as:
- Disparate Impact
- Demographic Parity
- False Positive Parity
- False Negative Parity
- Positive Prediction Value Parity
- Negative Prediction Value Parity
- Accuracy Parity

The fairness measures are computed based on a confusion matrix of a classification model. 


## Installation


Installing the package:
```
devtools::install_github("kozodoi/Fairness")
library("fairness")
```

Checking the list of implemented functions:
```
ls("package:fairness")
```


## Acknowledgments 
- Calders and Verwer (2010)
- Chouldechova (2017)
- Feldman et al. (2015)
- Friedler et al. (2018)
- Zafar et al. (2017)
