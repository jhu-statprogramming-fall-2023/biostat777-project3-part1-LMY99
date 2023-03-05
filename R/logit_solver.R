logit_log_likelihood <- function(coef, design, outcome) {
  linear_score <- design %*% coef
  if(is.numeric(outcome)){
  return(sum(
    outcome * -log1p(exp(-linear_score)) +
      (1 - outcome) * -log1p(exp(linear_score))
  ))
  }
  else if(is.list(outcome)){
    return(sum(
      outcome$n_success * -log1p(exp(-linear_score)) +
        (outcome$n_trial - outcome$n_success) * -log1p(exp(linear_score))
    ))
  }
}
logit_loglike_grad <- function(coef, design, outcome) {
  linear_score <- design %*% coef
  mean <- drop(1 / (1 + exp(-linear_score)))
  if(is.numeric(outcome)){
  return(drop(
    t(design) %*% (outcome - mean)
  ))
  }
  else if(is.list(outcome)){
    return(drop(
      t(design) %*% (outcome$n_success - mean*outcome$n_trial)
    ))
  }
}
logit_loglike_hessian <- function(coef, design, outcome) {
  linear_score <- design %*% coef
  mean <- drop(1 / (1 + exp(-linear_score)))
  mean_1m <- drop(1 / (1 + exp(linear_score)))
  if(is.numeric(outcome)){
  return(
    -t(design) %*% diag(mean * mean_1m) %*% design
  )
  }
  else if(is.list(outcome)){
    return(
      -t(design) %*% diag(mean * mean_1m * outcome$n_trial) %*% design
    )
  }
}
logit_bfgs <- function(design, outcome) {
  num_predictor <- ncol(design)
  init_coef <- rep(0, num_predictor)
  optim_result <- stats::optim(init_coef, logit_log_likelihood,
    logit_loglike_grad,
    design = design, outcome = outcome,
    method = "BFGS",
    control = list(fnscale = -1)
  )
  return(optim_result$par)
}
logit_newton <- function(design, outcome, option = list()) {
  num_predictor <- ncol(design)
  coef <- rep(0, num_predictor)
  n_max <- ifelse(is.null(option$n_iter), 20, option$n_iter)
  abs_tol <- ifelse(is.null(option$abs_tol), 1e-6, option$abs_tol)
  rel_tol <- ifelse(is.null(option$rel_tol), 1e-6, option$rel_tol)
  for (i in 1:n_max) {
    hessian <- logit_loglike_hessian(coef, design, outcome)
    grad <- logit_loglike_grad(coef, design, outcome)
    coef_new <- coef - drop(solve(hessian, grad))
    loglike_old <- logit_log_likelihood(coef, design, outcome)
    loglike_new <- logit_log_likelihood(coef_new, design, outcome)
    if (abs(loglike_new - loglike_old) <= abs_tol &&
      abs(loglike_new - loglike_old) <= rel_tol * max(loglike_old, loglike_new)) {
      break
    } else {
      coef <- coef_new
    }
  }
  return(coef)
}
