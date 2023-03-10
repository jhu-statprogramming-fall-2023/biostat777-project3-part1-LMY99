glm_bfgs <- function(design, outcome, loglike, loglike_grad, ...) {
  num_predictor <- ncol(design)
  init_coef <- rep(0, num_predictor)
  optim_result <- stats::optim(init_coef, loglike,
                               loglike_grad,
                               design = design, outcome = outcome,
                               method = "BFGS",
                               control = list(fnscale = -1), ...
  )
  return(optim_result$par)
}