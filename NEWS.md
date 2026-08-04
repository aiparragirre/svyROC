# svyROC 1.1.1

## Bug fixes

* `wroc()` did not emit the `(Sp = 1, Se = 0)` origin of the ROC curve when an
  observation with the event of interest had a predicted probability of exactly
  1. The largest cut-off in the grid was `1`, and sensitivity is thresholded
  with `phat >= cutoff`, so the first grid point already selected those
  observations. The shoelace sum used for `$wauc` equals the trapezoidal area
  only when the polygon closes at the origin, so the reported value was too low
  by exactly half the sensitivity at the first returned vertex. `wauc()` was
  unaffected.

  The grid now carries a cut-off above `max(phat.var)` when one is needed. When
  `max(phat.var) < 1` the returned cut-offs, sensitivities, specificities and
  `$wauc` are unchanged.

# svyROC 1.1.0

* Added new functions: `ci.wauc()`, `ht.paired()`, and `ht.indep()` to compute confidence intervals and perform hypothesis testing.

# svyROC 1.0.0

* Initial CRAN submission.
