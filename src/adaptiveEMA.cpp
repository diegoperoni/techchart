#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector adaptiveEMA(NumericVector prices, NumericVector alphas) {
    
    int i;
    Rcpp::NumericVector out = clone(prices);
    int n = out.size();
    
    /* Set leading NAs in output */
    for(i = 1; i < n; i++) {
      out[i] = NA_REAL;
    }
    
    /* Loop over non-NA input values */
    for(i = 1; i < n; i++) {
        out[i] = prices[i] * alphas[i] + out[i-1] * (1 - alphas[i]);
    }
    
    return out;
}
