logit <- function(z) {
  return(1 / (1 + exp(-z)))
}
are_all_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  are_all_within_atol <- all(abs_diff < abs_tol)
  are_all_within_rtol <- all(abs_diff < rel_tol * pmax(abs(v), abs(w)))
  return(are_all_within_atol && are_all_within_rtol)
}

logit_log_likelihood <- function(coef, design, outcome) {
  linear_score <- design %*% coef
  if (is.numeric(outcome)) {
    return(sum(
      outcome * -log1p(exp(-linear_score)) +
        (1 - outcome) * -log1p(exp(linear_score))
    ))
  } else if (is.list(outcome)) {
    return(sum(
      outcome$n_success * -log1p(exp(-linear_score)) +
        (outcome$n_trial - outcome$n_success) * -log1p(exp(linear_score))
    ))
  }
}
logit_loglike_grad <- function(coef, design, outcome) {
  linear_score <- design %*% coef
  mean <- drop(logit(linear_score))
  if (is.numeric(outcome)) {
    return(drop(
      t(design) %*% (outcome - mean)
    ))
  } else if (is.list(outcome)) {
    return(drop(
      t(design) %*% (outcome$n_success - mean * outcome$n_trial)
    ))
  }
}
logit_loglike_hessian <- function(coef, design, outcome) {
  linear_score <- design %*% coef
  mean <- drop(logit(linear_score))
  mean_1m <- drop(logit(-linear_score))
  if (is.numeric(outcome)) {
    return(
      -t(design) %*% diag(mean * mean_1m) %*% design
    )
  } else if (is.list(outcome)) {
    return(
      -t(design) %*% diag(mean * mean_1m * outcome$n_trial) %*% design
    )
  }
}
logit_newton_step <- function(current_coef, design, outcome,
                              option = list()){
  if(is.null(option$newton_solver) || option$newton_solver=='QR'){
    linear_score <- design %*% current_coef
    mean <- drop(logit(linear_score))
    mean_1m <- drop(logit(-linear_score))
    if (is.numeric(outcome)){
      v <- mean * mean_1m
      response <- linear_score + (outcome - mean)/v
    }
    else if(is.list(outcome)){
      v <- mean * mean_1m * outcome$n_trial
      response <- linear_score + (outcome$n_success - mean*outcome$n_trial)/v
    }
    design_mod <- diag(sqrt(v)) %*% design
    outcome_mod <- diag(sqrt(v)) %*% response
    new_coef <- qr_Eigen(design_mod, outcome_mod)

  } else if(option$newton_solver=='LU'){
  hessian <- logit_loglike_hessian(current_coef, design, outcome)
  grad <- logit_loglike_grad(current_coef, design, outcome)
  change <- drop(solve(hessian, grad))
  new_coef <- current_coef - change
  }
  return(new_coef)
}
logit_newton <- function(design, outcome, option = list()) {
  num_predictor <- ncol(design)
  coef <- rep(0, num_predictor)
  n_max <- ifelse(is.null(option$n_iter), 20, option$n_iter)
  abs_tol <- ifelse(is.null(option$abs_tol), 1e-6, option$abs_tol)
  rel_tol <- ifelse(is.null(option$rel_tol), 1e-6, option$rel_tol)
  for (i in 1:n_max) {
    coef_new <- logit_newton_step(coef, design, outcome, option)
    loglike_old <- logit_log_likelihood(coef, design, outcome)
    loglike_new <- logit_log_likelihood(coef_new, design, outcome)
    if (are_all_close(loglike_new, loglike_old, abs_tol, rel_tol)) {
      break
    } else {
      coef <- coef_new
    }
  }
  return(coef)
}
