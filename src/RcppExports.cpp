// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// adaptiveEMA
Rcpp::NumericVector adaptiveEMA(NumericVector prices, NumericVector alphas);
RcppExport SEXP _techchart_adaptiveEMA(SEXP pricesSEXP, SEXP alphasSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type prices(pricesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type alphas(alphasSEXP);
    rcpp_result_gen = Rcpp::wrap(adaptiveEMA(prices, alphas));
    return rcpp_result_gen;
END_RCPP
}
// backtest_cpp
Rcpp::DataFrame backtest_cpp(Rcpp::DataFrame data, bool orderside_long, int max_mins_wait);
RcppExport SEXP _techchart_backtest_cpp(SEXP dataSEXP, SEXP orderside_longSEXP, SEXP max_mins_waitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< bool >::type orderside_long(orderside_longSEXP);
    Rcpp::traits::input_parameter< int >::type max_mins_wait(max_mins_waitSEXP);
    rcpp_result_gen = Rcpp::wrap(backtest_cpp(data, orderside_long, max_mins_wait));
    return rcpp_result_gen;
END_RCPP
}
// cpt_trend
IntegerVector cpt_trend(NumericVector x, NumericVector y, int Q, long minseglen, double penalty);
RcppExport SEXP _techchart_cpt_trend(SEXP xSEXP, SEXP ySEXP, SEXP QSEXP, SEXP minseglenSEXP, SEXP penaltySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< int >::type Q(QSEXP);
    Rcpp::traits::input_parameter< long >::type minseglen(minseglenSEXP);
    Rcpp::traits::input_parameter< double >::type penalty(penaltySEXP);
    rcpp_result_gen = Rcpp::wrap(cpt_trend(x, y, Q, minseglen, penalty));
    return rcpp_result_gen;
END_RCPP
}
// houghtransform
DataFrame houghtransform(NumericVector x1, NumericVector y1, int flag, NumericVector rbucket, NumericVector abucket, int s);
RcppExport SEXP _techchart_houghtransform(SEXP x1SEXP, SEXP y1SEXP, SEXP flagSEXP, SEXP rbucketSEXP, SEXP abucketSEXP, SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y1(y1SEXP);
    Rcpp::traits::input_parameter< int >::type flag(flagSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type rbucket(rbucketSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type abucket(abucketSEXP);
    Rcpp::traits::input_parameter< int >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(houghtransform(x1, y1, flag, rbucket, abucket, s));
    return rcpp_result_gen;
END_RCPP
}
// ma
Rcpp::NumericVector ma(NumericVector x, int k, String weighting);
RcppExport SEXP _techchart_ma(SEXP xSEXP, SEXP kSEXP, SEXP weightingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    Rcpp::traits::input_parameter< String >::type weighting(weightingSEXP);
    rcpp_result_gen = Rcpp::wrap(ma(x, k, weighting));
    return rcpp_result_gen;
END_RCPP
}
// timesTwo
NumericVector timesTwo(NumericVector x);
RcppExport SEXP _techchart_timesTwo(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(timesTwo(x));
    return rcpp_result_gen;
END_RCPP
}
// findminima
IntegerMatrix findminima(NumericVector xmin, NumericVector xmax, NumericVector threshold);
RcppExport SEXP _techchart_findminima(SEXP xminSEXP, SEXP xmaxSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type xmin(xminSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(findminima(xmin, xmax, threshold));
    return rcpp_result_gen;
END_RCPP
}
// findminima_pt
IntegerMatrix findminima_pt(NumericVector xmin, NumericVector xmax, NumericVector threshold);
RcppExport SEXP _techchart_findminima_pt(SEXP xminSEXP, SEXP xmaxSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type xmin(xminSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(findminima_pt(xmin, xmax, threshold));
    return rcpp_result_gen;
END_RCPP
}
// findmaxima
IntegerMatrix findmaxima(NumericVector xmin, NumericVector xmax, NumericVector threshold);
RcppExport SEXP _techchart_findmaxima(SEXP xminSEXP, SEXP xmaxSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type xmin(xminSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(findmaxima(xmin, xmax, threshold));
    return rcpp_result_gen;
END_RCPP
}
// findmaxima_pt
IntegerMatrix findmaxima_pt(NumericVector xmin, NumericVector xmax, NumericVector threshold);
RcppExport SEXP _techchart_findmaxima_pt(SEXP xminSEXP, SEXP xmaxSEXP, SEXP thresholdSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type xmin(xminSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type threshold(thresholdSEXP);
    rcpp_result_gen = Rcpp::wrap(findmaxima_pt(xmin, xmax, threshold));
    return rcpp_result_gen;
END_RCPP
}
// sortoptimaposition
IntegerVector sortoptimaposition(IntegerVector pos, IntegerVector sign, NumericVector value);
RcppExport SEXP _techchart_sortoptimaposition(SEXP posSEXP, SEXP signSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type pos(posSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type sign(signSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(sortoptimaposition(pos, sign, value));
    return rcpp_result_gen;
END_RCPP
}
// sortoptimasign
IntegerVector sortoptimasign(IntegerVector pos, IntegerVector sign, NumericVector value);
RcppExport SEXP _techchart_sortoptimasign(SEXP posSEXP, SEXP signSEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type pos(posSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type sign(signSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(sortoptimasign(pos, sign, value));
    return rcpp_result_gen;
END_RCPP
}
// checkoptimasign
bool checkoptimasign(IntegerVector sign);
RcppExport SEXP _techchart_checkoptimasign(SEXP signSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type sign(signSEXP);
    rcpp_result_gen = Rcpp::wrap(checkoptimasign(sign));
    return rcpp_result_gen;
END_RCPP
}
// checkoptimapos
bool checkoptimapos(IntegerVector pos);
RcppExport SEXP _techchart_checkoptimapos(SEXP posSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type pos(posSEXP);
    rcpp_result_gen = Rcpp::wrap(checkoptimapos(pos));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_techchart_adaptiveEMA", (DL_FUNC) &_techchart_adaptiveEMA, 2},
    {"_techchart_backtest_cpp", (DL_FUNC) &_techchart_backtest_cpp, 3},
    {"_techchart_cpt_trend", (DL_FUNC) &_techchart_cpt_trend, 5},
    {"_techchart_houghtransform", (DL_FUNC) &_techchart_houghtransform, 6},
    {"_techchart_ma", (DL_FUNC) &_techchart_ma, 3},
    {"_techchart_timesTwo", (DL_FUNC) &_techchart_timesTwo, 1},
    {"_techchart_findminima", (DL_FUNC) &_techchart_findminima, 3},
    {"_techchart_findminima_pt", (DL_FUNC) &_techchart_findminima_pt, 3},
    {"_techchart_findmaxima", (DL_FUNC) &_techchart_findmaxima, 3},
    {"_techchart_findmaxima_pt", (DL_FUNC) &_techchart_findmaxima_pt, 3},
    {"_techchart_sortoptimaposition", (DL_FUNC) &_techchart_sortoptimaposition, 3},
    {"_techchart_sortoptimasign", (DL_FUNC) &_techchart_sortoptimasign, 3},
    {"_techchart_checkoptimasign", (DL_FUNC) &_techchart_checkoptimasign, 1},
    {"_techchart_checkoptimapos", (DL_FUNC) &_techchart_checkoptimapos, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_techchart(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
