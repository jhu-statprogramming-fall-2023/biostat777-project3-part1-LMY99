lm_log_likelihood <- function(coef, design, outcome, noise_var = 1) {
  prediction <- design %*% coef
  residual <- outcome - prediction
  return(sum(-residual^2 / noise_var / 2))
}
lm_loglike_grad <- function(coef, design, outcome, noise_var = 1) {
  prediction <- design %*% coef
  residual <- outcome - prediction
  return(drop(crossprod(design, residual) / noise_var))
}
lm_bfgs <- function(design, outcome, noise_var = 1) {
  num_predictor <- ncol(design)
  init_coef <- rep(0, num_predictor)
  optim_result <- optim(init_coef, lm_log_likelihood, lm_loglike_grad,
    design = design, outcome = outcome, method = "BFGS",
    control = list(fnscale = -1)
  )
  return(optim_result$par)
}
