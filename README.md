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
- Calders, T., & Verwer, S. (2010). Three naive Bayes approaches for discrimination-free classification. Data Mining and Knowledge Discovery, 21(2), 277-292.
- Chouldechova, A. (2017). Fair prediction with disparate impact: A study of bias in recidivism prediction instruments. Big data, 5(2), 153-163.
- Feldman, M., Friedler, S. A., Moeller, J., Scheidegger, C., & Venkatasubramanian, S. (2015, August). Certifying and removing disparate impact. In Proceedings of the 21th ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (pp. 259-268). ACM.
- Friedler, S. A., Scheidegger, C., Venkatasubramanian, S., Choudhary, S., Hamilton, E. P., & Roth, D. (2018). A comparative study of fairness-enhancing interventions in machine learning. arXiv preprint arXiv:1802.04422.
- Zafar, M. B., Valera, I., Gomez Rodriguez, M., & Gummadi, K. P. (2017, April). Fairness beyond disparate treatment & disparate impact: Learning classification without disparate mistreatment. In Proceedings of the 26th International Conference on World Wide Web (pp. 1171-1180). International World Wide Web Conferences Steering Committee.
