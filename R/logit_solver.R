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
