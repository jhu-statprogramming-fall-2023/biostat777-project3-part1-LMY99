#include <Rcpp.h>
#include "hiperglm_types.h"
// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

// [[Rcpp::export]]
VectorXd qr_Eigen(const MatrixXd& A, const VectorXd& b){
  matqr qr_A(A);
  VectorXd z(qr_A.solve(b));
  return z;
}
