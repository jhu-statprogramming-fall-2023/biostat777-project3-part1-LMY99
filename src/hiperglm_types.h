#ifndef HIPERGLM_TYPES_H
#define HIPERGLM_TYPES_H

#include <RcppEigen.h>
using Eigen::Solve;
using Eigen::MatrixXd;
using Eigen::VectorXd;
using Eigen::HouseholderQR;
typedef HouseholderQR<MatrixXd> matqr;
typedef Solve<matqr,VectorXd> qrresult;

VectorXd qr_Eigen(const MatrixXd& A, const VectorXd& b);

#endif
