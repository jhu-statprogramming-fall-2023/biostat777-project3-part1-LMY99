lm_pseudo_inverse <- function(design, outcome) {
  drop(solve(
    crossprod(design),
    crossprod(design, outcome)
  ))
}
