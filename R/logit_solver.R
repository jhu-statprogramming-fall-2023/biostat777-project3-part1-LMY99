logit_log_likelihood <- function(coef, design, outcome){
  linear_score <- design %*% coef
  return(sum(
    outcome * -log1p(exp(-linear_score)) +
    (1-outcome) * -log1p(exp(linear_score))
  ))
}
logit_loglike_grad <- function(coef, design, outcome){
  linear_score <- design %*% coef
  mean <- drop(1/(1+exp(-linear_score)))
  return(drop(
    t(design) %*% (outcome - mean)
  ))
}
logit_loglike_hessian <- function(coef, design, outcome){
  linear_score <- design %*% coef
  mean <- drop(1/(1+exp(-linear_score)))
  mean_1m <- drop(1/(1+exp(linear_score)))
  return(
    - t(design) %*% diag(mean*mean_1m) %*% design
  )
}
logit_bfgs <- function(design, outcome){
  num_predictor <- ncol(design)
  init_coef <- rep(0, num_predictor)
  optim_result <- stats::optim(init_coef, logit_log_likelihood,
                               logit_loglike_grad,
                               design=design, outcome=outcome,
                               method="BFGS",
                               control=list(fnscale=-1))
  return(optim_result$par)
}
logit_newton <- function(design, outcome, option=list(n_iter=50)){
  num_predictor <- ncol(design)
  coef <- rep(0, num_predictor)
  for(i in 1:option$n_iter){
    hessian <- logit_loglike_hessian(coef, design, outcome)
    grad <- logit_loglike_grad(coef, design, outcome)
    coef <- coef - drop(solve(hessian, grad))
  }
  return(coef)
}
