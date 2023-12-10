#' @export
print.hglm <- function(hglm_out) {
  cat("Output of hiper_glm:\n")
  # TODO: Display model information
  cat(paste("Solver:", hglm_out$solver))
  cat("Model Coefficients:\n")
  cat(hglm_out$coefficients)
  warning("print.hglm not yet implemented.")
}
#' @export
coef.hglm <- function(hglm_out) {
  hglm_out$coefficients
}

vcov.hglm <- function(hglm_out) {
  # TODO: Return covariance matrix of MLE
  warning("vcov.hglm not yet implemented.")
}
