lm_pseudo_inverse <- function(design, outcome) {
  qr.solve(design, outcome)
}
