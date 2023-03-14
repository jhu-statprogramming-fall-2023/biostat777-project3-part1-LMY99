testthat::test_that("Eigen and base-R QR least square solver coincides",
  {
    sim <- simulate_data(32, 4, seed=314)
    A <- sim$design
    b <- sim$outcome
    ols_eigen <- qr_Eigen(A,b)
    ols_baseR <- qr.solve(A,b)
    expect_true(are_all_close(ols_eigen, ols_baseR))
  }
)
