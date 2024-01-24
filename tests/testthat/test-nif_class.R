test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("print works with minimal NIF" , {
  expect_no_error(print(examplinib_poc_min_nif))
  expect_no_error(print(examplinib_sad_min_nif))
})


test_that("subjects works with minimal NIF" , {
  expect_no_error(subjects(examplinib_poc_nif))
  expect_no_error(subjects(examplinib_poc_min_nif))
  expect_equal(ncol(subjects(examplinib_poc_nif)), 2)
  expect_equal(ncol(subjects(examplinib_poc_min_nif)), 2)
})


# test_that("subject_info works with minimal NIF" , {
#   expect_no_error(subject_info(examplinib_poc_min_nif, 1))
#   expect_no_error(subject_info(examplinib_sad_min_nif, 1))
# })
