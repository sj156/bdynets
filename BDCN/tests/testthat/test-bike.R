test_that("bundled bike data reproduce the frozen design", {
  bike <- bdcn_bike_data()
  expect_s3_class(bike, "bdcn_bike_data")
  expect_equal(dim(bike$Y), c(1448L, 24L))
  expect_equal(dim(bike$covariates), c(1448L, 13L))
  expect_identical(bike$split$training, 13:960)
  expect_identical(bike$split$validation, 961:1208)
  expect_identical(bike$split$test, 1209:1448)
  expect_equal(nrow(bdcn_all_pairs(bike$graph, bdcn_bike_control("smoke"))),
               576L)
})

test_that("bike smoke profile runs fit, DSS, and rolling prediction", {
  skip_on_cran()
  result <- bdcn_run_bike_example("smoke")
  expect_s3_class(result$fit, "bdcn_general_fit")
  expect_s3_class(result$dss, "bdcn_dss")
  expect_equal(nrow(result$scores), 2L)
  expect_equal(nrow(result$selection), 576L)
  expect_equal(nrow(result$prediction_table), 2L * 240L * 24L)
  expect_true(all(is.finite(result$scores$PoissonDeviance)))
})

