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
