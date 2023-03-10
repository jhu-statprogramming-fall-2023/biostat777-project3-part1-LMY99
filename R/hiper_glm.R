#' @export
hiper_glm <- function(design, outcome, model = "linear", option = list()) {
  support_model <- c("linear", "logit")
  if (!(model %in% support_model)) {
    stop(sprintf("Model %s not supported.", model))
  }
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  if (model == "linear") {
    if (is.null(option$mle_solver) || option$mle_solver == "PINV") {
      hglm_out$coefficients <- lm_pseudo_inverse(design, outcome)
      hglm_out$mle_solver <- "PSEUDO_INVERSE"
    } else if (option$mle_solver == "BFGS") {
      hglm_out$coefficients <- glm_bfgs(
        design, outcome,
        lm_log_likelihood,
        lm_loglike_grad
      )
      hglm_out$mle_solver <- "BFGS"
    } else {
      stop("MLE solver must be PINV or BFGS.")
    }
  } else if (model == "logit") {
    if (is.null(option$mle_solver) || option$mle_solver == "NEWTON") {
      hglm_out$coefficients <- logit_newton(design, outcome, option = option)
      hglm_out$mle_solver <- "NEWTON"
    } else if (option$mle_solver == "BFGS") {
      hglm_out$coefficients <- glm_bfgs(
        design, outcome,
        logit_log_likelihood,
        logit_loglike_grad
      )
      hglm_out$mle_solver <- "BFGS"
    } else {
      stop("MLE solver must be NEWTON or BFGS.")
    }
  }
  return(hglm_out)
}
