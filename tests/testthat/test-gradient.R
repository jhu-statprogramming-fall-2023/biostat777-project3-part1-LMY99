testthat::test_that(
  "Numerical and analytical gradients coincide",
  {
    n_obs <- 32
    n_pred <- 4
    data <- simulate_data(n_obs, n_pred, model = "linear", seed = 150)
    design <- data$design
    outcome <- data$outcome
    coef_true <- data$coef_true
    analytic_grad <- lm_loglike_grad(coef_true, design, outcome)
    numeric_grad <- approx_grad(lm_log_likelihood, coef_true,
      design = design, outcome = outcome
    )
    testthat::expect_true(are_all_close(analytic_grad, numeric_grad))
  }
)
