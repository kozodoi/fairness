# fairness 1.2.2
- more detailed error messages for factor level mismatch
- fixed errors for some factor outcomes

# fairness 1.2.1
- fixed inconsistencies with `outcome_base` values
- removed `preds_levels` argument
- improved documentation of metric functions

# fairness 1.2.0
- added support for continuous group feature with binning options
- added group size to metric outputs
- small updates in documentation

# fairness 1.1.1
- added check and conversion of data to `data.frame` in metric functions
- quiet AUC computation in `roc_parity()`
- small updates in documentation

# fairness 1.1.0
- fixed `outcome_levels` issue when levels of provided predictions do not match outcome levels
- renamed `outcome_levels` to `preds_levels` to improve clarity
- added `outcome_base` argument to set base level for target variable used to compute fairness metrics
- fixed `fnr_parity()` and `fpr_parity()` calculations for different outcome bases
- updates in package documentation

# fairness 1.0.2
- small fixes in documentation

# fairness 1.0.1
- CRAN resubmission of fairness
- fix of `DESCRIPTION` and `LICENSE` files

# fairness 1.0.0
- the first stable version of fairness
