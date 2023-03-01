testthat::test_that(
  "Function returns TRUE when vectors are close",
  {
    vec1 <- c(1, 1.5, 2)
    # The absolute error is 1e-5
    # Since abs(vec1) > 1, relative error is at most 1e-5
    vec2 <- vec1 + 1e-5
    testthat::expect_true(
      are_all_close(vec1, vec2, abs_tol = 1e-3, rel_tol = 1e-3)
    )
  }
)
testthat::test_that(
  "Function returns FALSE when absolute error exceeds limit",
  {
    vec1 <- c(1, 1.5, 2)
    # The absolute error is 1e-5
    # Since abs(vec1) > 1, relative error is at most 1e-5
    vec2 <- vec1 + 1e-5
    testthat::expect_false(
      are_all_close(vec1, vec2, abs_tol = 1e-7, rel_tol = 1e-3)
    )
  }
)
testthat::test_that(
  "Function returns FALSE when relative error exceeds limit",
  {
    vec1 <- c(1, 1.5, 2)
    # The absolute error is 2e-5
    # Since abs(vec1) < 2, relative error is at least 1e-5
    vec2 <- vec1 + 2e-5
    testthat::expect_false(
      are_all_close(vec1, vec2, abs_tol = 1e-3, rel_tol = 1e-7)
    )
  }
)
