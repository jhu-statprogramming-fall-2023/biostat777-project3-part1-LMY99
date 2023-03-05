testthat::test_that(
  "Numerical and analytical gradients coincide for linear model",
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

testthat::test_that(
  "Numerical and analytical gradients coincide for logit model",
  {
    n_obs <- 32
    n_pred <- 4
    data <- simulate_data(n_obs, n_pred, model = "logit", seed = 150)
    design <- data$design
    outcome <- data$outcome
    coef_true <- data$coef_true
    analytic_grad <- logit_loglike_grad(coef_true, design, outcome)
    numeric_grad <- approx_grad(logit_log_likelihood, coef_true,
      design = design, outcome = outcome
    )
    testthat::expect_true(are_all_close(analytic_grad, numeric_grad))
  }
)

testthat::test_that(
  "Numerical and analytical hessians coincide for logit model",
  {
    n_obs <- 32
    n_pred <- 4
    data <- simulate_data(n_obs, n_pred, model = "logit", seed = 340)
    design <- data$design
    outcome <- data$outcome
    coef_true <- data$coef_true
    analytic_hess <- logit_loglike_hessian(coef_true, design, outcome)
    for (i in 1:n_pred) {
      direction <- rep(0, n_pred)
      direction[i] <- 1
      direction_grad <- function(t) {
        logit_loglike_grad(coef_true + t * direction, design, outcome)
      }
      numeric_hess_multiply <- rep(0, n_pred)
      for (j in 1:n_pred) {
        numeric_hess_multiply[j] <-
          approx_grad(function(t) direction_grad(t)[j], 0)
      }
      analytic_hess_multiply <- analytic_hess %*% direction
      testthat::expect_true(are_all_close(
        analytic_hess_multiply,
        numeric_hess_multiply
      ))
    }
  }
)
