test_that("GLS and reusable state-space APIs belong to the bdynets module namespace", {
  namespace <- asNamespace("bdynets")
  entry_points <- c("gls_control", "gls_fit", "gls_fit_general", "gls_study_control",
                    "gls_fit_study", "gaussian_ffbs", "poisson_discount_filter")
  expect_true(all(entry_points %in% getNamespaceExports("bdynets")))
  for (name in entry_points) {
    expect_identical(environment(getExportedValue("bdynets", name)), namespace,
                     info = name)
  }
  description <- utils::packageDescription("bdynets")
  dependency_text <- paste(unlist(description[c("Depends", "Imports", "LinkingTo")]),
                           collapse = ",")
  dependencies <- unlist(strsplit(dependency_text, "[[:space:],()]+"))
  expect_false(any(c("GLS", "mcmcKernels") %in% dependencies))
  routines <- getDLLRegisteredRoutines("bdynets")$.Call
  expected <- c("_bdynets_ffbs_scalar_cpp", "_bdynets_poisson_reference_cpp",
                "_bdynets_gls_ffbs_rank1_cpp", "_bdynets_gls_reference_path_native")
  expect_true(all(expected %in% names(routines)))
  expect_equal(routines[["_bdynets_gls_reference_path_native"]]$numParameters, 6L)

  # These calls need only the bdynets namespace and its own native library.
  # An isolated package installation exercises this when no old GLS or kernel
  # package is present, without prohibiting those packages on a user's machine.
  cfg <- gls_control("smoke", backend = "cpp")
  expect_identical(cfg$backend, "cpp")
  expect_identical(cfg$reference_backend, "native")
  Y <- matrix(c(2, 3, 1, 4, 2, 2), 3L, 2L)
  H <- array(.2, c(3L, 2L, 1L))
  reference <- bdynets:::.gls_reference_path_native(Y, H,
    matrix(log(2), 3L, 2L), .95, c(0, 1), TRUE)
  expect_true(all(is.finite(unlist(reference))))
  expect_length(reference$mean_path, nrow(Y))
  expect_true(all(reference$W > 0))
  set.seed(909)
  draw <- gaussian_ffbs(c(.2, -.1, .4), loadings = .7, precision = 3,
                        innovation_variance = .1, transition = -.4,
                        initial_mean = 1.2, initial_variance = .6)
  expect_length(draw$state, 3L)
  expect_true(all(is.finite(c(draw$state, draw$initial))))
  filter <- poisson_discount_filter(c(2, 3, 1), loadings = .2, offset = log(2),
                                     initial_mean = .3, initial_variance = .7)
  expect_length(filter$filtered_mean, 3L)
  expect_true(all(is.finite(unlist(filter))))
})
