## Resubmission
This is a resubmission. In this version I have:

* Updated version number to 1.1.0
* Fixed `outcome_levels` issue when levels of provided predictions do not match outcome levels
* Renamed `outcome_levels` to `preds_levels` to imporve clarity
* Added `outcome_base` argument to fairness metric functions to set base level for target varible used to compute fairness metrics
* Updated the documentation icluding README and vignettes


## Test environments
* local OS X install, R 3.6.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (R-devel, R-release)


## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.


## Downstream dependencies
There are currently no downstream dependencies for this package.