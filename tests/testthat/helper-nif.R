# Build a nif object the same way production code does (tibble + nif_version),
# without nif() validation or row reordering.
as_nif_test <- function(df) {
  nif:::new_nif(df)
}


as_domain_test <- function(df, name = "", trial_title = "", studyid = "") {
  nif:::new_domain(
    df,
    name = name,
    trial_title = trial_title,
    studyid = studyid
  )
}


# Snapshot .nif_env, optionally set options, and restore when the calling
# frame exits (testthat block or on error).
local_nif_option <- function(..., .envir = parent.frame()) {
  env <- nif:::.nif_env
  old <- as.list(env)
  if (...length() > 0) {
    nif_option(...)
  }
  withr::defer(
    {
      rm(list = ls(envir = env), envir = env)
      list2env(old, envir = env)
    },
    envir = .envir
  )
  invisible(old)
}
