# Build a nif object the same way production code does (tibble + nif_version),
# without nif() validation or row reordering.
as_nif_test <- function(df) {
  nif:::new_nif(df)
}
