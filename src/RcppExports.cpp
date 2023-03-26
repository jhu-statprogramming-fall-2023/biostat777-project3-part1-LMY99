// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "hiperglm_types.h"
#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// qr_Eigen
VectorXd qr_Eigen(const MatrixXd& A, const VectorXd& b);
RcppExport SEXP _hiperglm_qr_Eigen(SEXP ASEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const MatrixXd& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const VectorXd& >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(qr_Eigen(A, b));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_hiperglm_qr_Eigen", (DL_FUNC) &_hiperglm_qr_Eigen, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_hiperglm(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
