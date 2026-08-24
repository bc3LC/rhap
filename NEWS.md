<!-- ------------------------>
<!-- ------------------------>
# rhap 0.1.0
<p align="center"> <img src="READMEfigs/metisHeaderThick.PNG"></p>
<!-- ------------------------>
<!-- ------------------------>

## New features

* `fit_model()` re-specified: linear year trend replaces year fixed effects, NMVOC dropped (non-significant under every specification/SE estimator tested), and Driscoll-Kraay standard errors are now computed and returned (`vcov_dk`), justified by confirmed serial correlation and cross-sectional dependence in the panel.
* The country-level bias-correction term is now computed internally by `fit_model()` from the fitted model itself (`bias_adder$bias.adder`), replacing the static `rhap::hia_adder` dataset, which has been removed along with `calc_hia_adder()`.
* `by_gr = TRUE` in `calc_hap_impacts()` now reports within-country income-decile group *shares* of population, PM2.5+NOx exposure, and GDP (table + pie chart), instead of a fabricated group-level health-impact estimate. The regression model is fit on between-country variation only, so applying it to within-country decile inputs was an unvalidated extrapolation; shares of GCAM's own group-level data carry no such risk.
* `fit_model()`'s `bias_adder` now also returns `reliability_ratio` (and `calc_hap_impacts()`'s output a derived `reliability` "high"/"medium"/"low" label), flagging countries whose absolute-level prediction rests mostly on the bias correction rather than the regression's own covariates -- computed already, previously discarded.
* `calc_hap_impacts()` gains a `fit_result` argument: pass a previously-computed `fit_model(HIA_var)` result to skip refitting the national regression (including its Driscoll-Kraay vcov) on every call, e.g. when comparing many GCAM scenarios for the same `HIA_var`.
* Added a new "Validating the econometric model" vignette: a reproducible record of the panel/inference diagnostics, the (rejected) GDP x PM2.5 interaction test, the additive-vs-multiplicative bias-adder investigation, and the GBD real-data validation methodology.

## Bug Fixes

* Fixed the country-level bias correction silently clamping a negative "predicted" death/YLL/DALY rate to exactly 0 (occurred for ~12% of observations on the model's own training data). It now floors at a small fraction of the naive, uncorrected prediction instead. (A multiplicative bias correction, which prevents negative values by construction, was also tried; it was rejected after validation against real GBD data showed it performing far worse than the additive form -- see `fit_model()`'s documentation and the econometric-model vignette.)
* Fixed `fit_model(HIA_var = "yll")`: valid input silently resolved to `NULL` due to a mismatched lookup key (`"ylls"` vs. `"yll"`).
* Fixed a rerun of `calc_hap_impacts(map = TRUE)` against a not-freshly-cleared `output/` directory failing because the map-output file mover matched, and tried to rename, its own destination folder.
* Fixed several `.RData` test snapshots (`calc_hap_impacts`, `calc_ResidEm_grp` x2, `fit_model`) being compared vacuously: `load()` injects an object under whatever name it was *saved* as, which for these files happened to match the local variable holding the freshly-computed test result (`testOutput`), silently overwriting it before `expect_equal()` ran -- so the test was comparing the stale snapshot to itself, regardless of what the function under test actually returned. Also removed the now-redundant, structurally-brittle `fit_model` snapshot in favor of targeted structural/sign/parameter checks.

## Package/performance

* Removed dead code (`flsp_pc_gr`, `flsp_pc_ctry_gr`, `gdp_pc_ctry_gr`) left over from the `by_gr` redesign.
* GCAM group-level queries (`subregional population`, `subregional income`) and their downstream processing now only run when `by_gr = TRUE` (the shares they feed are otherwise never consumed) -- skips two extra GCAM queries and their processing on every default-mode (`by_gr = FALSE`) call.