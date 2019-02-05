# Fairness

## Description

This package contains functions to compute measures of algorithmic fairness such as:
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


## Example

1) Loading the sample data set:
```
df = fairness::compas
head(df)
```

2) Generating predicted probabilities:
```
probs = df$score
summary(probs)
```

3) Computing accuracy parity for race:
```
acc_parity(actuals = df$label_value, predicted = probs, group = df$race, base = "Caucasian")
```


## Acknowledgments 
- Calders and Verwer (2010)
- Chouldechova (2017)
- Feldman et al. (2015)
- Friedler et al. (2018)
- Zafar et al. (2017)
