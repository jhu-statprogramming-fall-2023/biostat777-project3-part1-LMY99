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
      hglm_out$coefficients <- lm_bfgs(design, outcome)
      hglm_out$mle_solver <- "BFGS"
    }
  }
  else if (model == "logit"){
    if(is.null(option$mle_solver) || option$mle_solver == "BFGS"){
      hglm_out$coefficients <- logit_bfgs(design, outcome)
      hglm_out$mle_solver <- "BFGS"
    }
    else if(option$mle_solver == "NEWTON"){
      hglm_out$coefficients <- logit_newton(design, outcome, option=option)
      hglm_out$mle_solver <- "NEWTON"
    }
  }
  return(hglm_out)
}
