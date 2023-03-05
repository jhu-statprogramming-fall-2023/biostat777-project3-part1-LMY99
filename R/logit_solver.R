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
