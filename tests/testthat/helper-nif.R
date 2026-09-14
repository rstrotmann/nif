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
