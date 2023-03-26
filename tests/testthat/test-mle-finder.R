test_that("linalg and optim least-sq coincide", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "linear", seed = 0)
  design <- data$design
  outcome <- data$outcome
  via_linalg_out <- hiper_glm(design, outcome, model = "linear")
  via_bfgs_out <- hiper_glm(
    design, outcome,
    model = "linear", option = list(mle_solver = "BFGS")
  )
  expect_true(are_all_close(
    coef(via_linalg_out), coef(via_bfgs_out),
    abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that("newton and bfgs outputs coincide on logit model", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "logit", seed = 1918)
  design <- data$design
  outcome <- data$outcome
  via_newton_out <- hiper_glm(design, outcome, model = "logit")
  via_bfgs_out <- hiper_glm(
    design, outcome,
    model = "logit", option = list(mle_solver = "BFGS")
  )
  expect_true(are_all_close(
    coef(via_newton_out), coef(via_bfgs_out),
    abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that("newton and bfgs outputs coincide on binomial model", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred,
    model = "logit", seed = 1918,
    option = list(n_trial = 1:n_obs)
  )
  design <- data$design
  outcome <- data$outcome
  via_newton_out <- hiper_glm(design, outcome, model = "logit")
  via_bfgs_out <- hiper_glm(
    design, outcome,
    model = "logit", option = list(mle_solver = "BFGS")
  )
  expect_true(are_all_close(
    coef(via_newton_out), coef(via_bfgs_out),
    abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that("Bernoulli and binomial with n_trial=1 have matching output", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred,
    model = "logit", seed = 1918,
    option = list(n_trial = rep(1, n_obs))
  )
  design <- data$design
  outcome <- data$outcome
  via_newton_out_binary <- hiper_glm(design, outcome, model = "logit")
  via_bfgs_out_binary <- hiper_glm(
    design, outcome,
    model = "logit", option = list(mle_solver = "BFGS")
  )
  via_newton_out_binom <- hiper_glm(design, outcome$n_success, model = "logit")
  via_bfgs_out_binom <- hiper_glm(
    design, outcome$n_success,
    model = "logit", option = list(mle_solver = "BFGS")
  )
  expect_true(are_all_close(
    coef(via_newton_out_binom), coef(via_newton_out_binary),
    abs_tol = 1e-2, rel_tol = 1e-2
  ))
  expect_true(are_all_close(
    coef(via_bfgs_out_binom), coef(via_bfgs_out_binary),
    abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that("newton methods have same results with LU and QR solver on logit model", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = "logit", seed = 1918)
  design <- data$design
  outcome <- data$outcome
  via_newton_qr_out <- hiper_glm(design, outcome, model = "logit",
                              option = list(mle_solver = "NEWTON", newton_solver = "QR"))
  via_newton_lu_out <- hiper_glm(
    design, outcome,
    model = "logit", option = list(mle_solver = "NEWTON", newton_solver = "LU")
  )
  expect_true(are_all_close(
    coef(via_newton_qr_out), coef(via_newton_lu_out),
    abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that("newton methods have same results with LU and QR solver on binomial model", {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred,
                        model = "logit", seed = 1918,
                        option = list(n_trial = 1:n_obs)
  )
  design <- data$design
  outcome <- data$outcome
  via_newton_qr_out <- hiper_glm(design, outcome, model = "logit",
                                 option = list(mle_solver = "NEWTON", newton_solver = "QR"))
  via_newton_lu_out <- hiper_glm(
    design, outcome,
    model = "logit", option = list(mle_solver = "NEWTON", newton_solver = "LU")
  )
  expect_true(are_all_close(
    coef(via_newton_qr_out), coef(via_newton_lu_out),
    abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
