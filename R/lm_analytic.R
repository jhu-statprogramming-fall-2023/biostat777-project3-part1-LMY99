lm_pseudo_inverse <- function(design, outcome) {
  qr_Eigen(design, outcome)
}
