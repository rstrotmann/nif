# test_that("nif_nca_new works with basic input", {
#   # Create a simple NIF object with one subject and one analyte
#   nif_data <- data.frame(
#     USUBJID = c("SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1"),
#     TIME = c(0, 1, 2, 4, 8, 12),
#     DV = c(0, 10, 20, 15, 8, 4),
#     EVID = c(1, 0, 0, 0, 0, 0),
#     AMT = c(100, 0, 0, 0, 0, 0),
#     ANALYTE = "DRUG1"
#   )
#   class(nif_data) <- c("nif", "data.frame")
#
#   # Run NCA
#   results <- nif_nca_new(nif_data)
#
#   # Check basic structure
#   expect_s3_class(results, "data.frame")
#   expect_true(all(c("USUBJID", "ANALYTE", "PPTESTCD", "PPSTRESN") %in% names(results)))
#   expect_true(nrow(results) > 0)
# })
#
# test_that("nif_nca_new handles multiple analytes", {
#   # Create NIF object with two analytes
#   nif_data <- data.frame(
#     USUBJID = rep(c("SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1"), 2),
#     TIME = rep(c(0, 1, 2, 4, 8, 12), 2),
#     DV = c(0, 10, 20, 15, 8, 4, 0, 5, 10, 8, 4, 2),
#     EVID = rep(c(1, 0, 0, 0, 0, 0), 2),
#     AMT = rep(c(100, 0, 0, 0, 0, 0), 2),
#     ANALYTE = rep(c("DRUG1", "DRUG2"), each = 6)
#   )
#   class(nif_data) <- c("nif", "data.frame")
#
#   # Run NCA for both analytes
#   results <- nif_nca_new(nif_data)
#
#   # Check that both analytes are processed
#   expect_equal(length(unique(results$ANALYTE)), 2)
#   expect_true(all(c("DRUG1", "DRUG2") %in% results$ANALYTE))
# })
#
# test_that("nif_nca_new handles specified parameters", {
#   nif_data <- data.frame(
#     USUBJID = c("SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1"),
#     TIME = c(0, 1, 2, 4, 8, 12),
#     DV = c(0, 10, 20, 15, 8, 4),
#     EVID = c(1, 0, 0, 0, 0, 0),
#     AMT = c(100, 0, 0, 0, 0, 0),
#     ANALYTE = "DRUG1"
#   )
#   class(nif_data) <- c("nif", "data.frame")
#
#   # Run NCA with specific parameters
#   results <- nif_nca_new(nif_data, parameters = c("cmax", "tmax"))
#
#   # Check that only specified parameters are calculated
#   expect_equal(length(unique(results$PPTESTCD)), 2)
#   expect_true(all(c("cmax", "tmax") %in% results$PPTESTCD))
# })
#
# test_that("nif_nca_new handles additional columns", {
#   nif_data <- data.frame(
#     USUBJID = c("SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1", "SUBJ1"),
#     TIME = c(0, 1, 2, 4, 8, 12),
#     DV = c(0, 10, 20, 15, 8, 4),
#     EVID = c(1, 0, 0, 0, 0, 0),
#     AMT = c(100, 0, 0, 0, 0, 0),
#     ANALYTE = "DRUG1",
#     DOSE = 100,
#     SEX = "M"
#   )
#   class(nif_data) <- c("nif", "data.frame")
#
#   # Run NCA with additional columns
#   results <- nif_nca_new(nif_data, keep = c("DOSE", "SEX"))
#
#   # Check that additional columns are included
#   expect_true(all(c("DOSE", "SEX") %in% names(results)))
# })
#
# test_that("nif_nca_new handles empty input", {
#   # Create empty NIF object
#   empty_nif <- new_nif()
#
#   # Run NCA
#   results <- nif_nca_new(empty_nif)
#
#   # Check that results are empty but have correct structure
#   expect_s3_class(results, "data.frame")
#   expect_equal(nrow(results), 0)
#   expect_true(all(c("USUBJID", "ANALYTE", "PPTESTCD", "PPSTRESN") %in% names(results)))
# })
#
# test_that("nif_nca_new handles subjects with no observations", {
#   # Create NIF object with one subject having only dose
#   nif_data <- data.frame(
#     USUBJID = c("SUBJ1", "SUBJ2"),
#     TIME = c(0, 0),
#     DV = c(0, 0),
#     EVID = c(1, 1),
#     AMT = c(100, 100),
#     ANALYTE = "DRUG1"
#   )
#   class(nif_data) <- c("nif", "data.frame")
#
#   # Run NCA
#   results <- nif_nca_new(nif_data)
#
#   # Check that no results are generated for subjects with no observations
#   expect_equal(nrow(results), 0)
# })
#
# test_that("nif_nca_new handles invalid input", {
#   # Test with non-NIF object
#   expect_error(nif_nca_new(data.frame()), "Input must be a NIF object")
#
#   # Test with NULL input
#   expect_error(nif_nca_new(NULL), "Input must be a NIF object")
# })
