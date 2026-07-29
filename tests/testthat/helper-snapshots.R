# Load an .RData snapshot without clobbering same-named variables in the
# caller's environment. get(load(path)) is unsafe: load() injects the
# saved object under whatever name it was saved as directly into the
# calling environment as a side effect, independent of what the load()
# return value (the object's name) is assigned to. If a snapshot was
# saved under the same name as the local variable holding a freshly
# computed result (e.g. "testOutput"), load() silently overwrites that
# variable before the comparison runs -- turning expect_equal(testOutput,
# testResult) into a comparison of the stale snapshot against itself.
# This happened for several snapshots in this suite (calc_hap_impacts,
# calc_ResidEm_grp x2, fit_model), which meant those tests always passed
# regardless of what the function under test actually returned.
load_snapshot <- function(path) {
  e <- new.env()
  nm <- load(path, envir = e)
  get(nm[1], envir = e)
}
