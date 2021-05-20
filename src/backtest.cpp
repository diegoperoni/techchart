#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
Rcpp::DataFrame backtest_cpp(Rcpp::DataFrame data, bool orderside_long, int max_mins_wait) {
  
  Rcpp::DataFrame out = Rcpp::clone(data);
  
  // input
  Rcpp::IntegerVector  enter = data["actions"];
  Rcpp::NumericVector  close = data["close"];
  Rcpp::NumericVector  low = data["low"];
  Rcpp::NumericVector  high = data["high"];
  Rcpp::DatetimeVector date = data["date"];
  Rcpp::NumericVector  true_close = data["true.close"];
  
  // output
  Rcpp::NumericVector   EnterPrice = data["Enter.Price"];
  Rcpp::CharacterVector EnterDate = data["Enter.Date"];
  Rcpp::NumericVector   ExitPrice = data["Exit.Price"];
  Rcpp::CharacterVector ExitDate = data["Exit.Date"];
  Rcpp::CharacterVector EnterTape = data["Enter.Tape"];
  Rcpp::CharacterVector ExitTape = data["Exit.Tape"];
  Rcpp::NumericVector   TruePrice = data["True.Price"];
  
  int pos = 0;
  int placeholder_time_wait = 0;
  double lmt_price = 0;
  
  for (int i=0; i<data.nrow(); i++) {
    if (pos == 0) {
      if (enter[i] == 1) { // entrata LMT
        lmt_price = close[i];
        placeholder_time_wait = max_mins_wait;
        
      } else if (enter[i] == 2) { //entrata a MKT
        EnterPrice[i] = close[i];
        EnterDate[i] = date[i];
        EnterTape[i] = "ENTER.MKT";
        TruePrice[i] = true_close[i];
        pos = 1;
        placeholder_time_wait = 0;
        
      } else {
        if (placeholder_time_wait>0) { // find potential entry to lmt_price
          if (orderside_long) {
            if (low[i] < lmt_price) {
              EnterPrice[i] = lmt_price;
              EnterDate[i] = date[i];
              EnterTape[i] = "ENTER.LMT";
              TruePrice[i] = true_close[i];
              pos = 1;
              placeholder_time_wait = 0;
            }
          } else {
            if (high[i] > lmt_price) {
              EnterPrice[i] = lmt_price;
              EnterDate[i] = date[i];
              EnterTape[i] = "ENTER.LMT";
              TruePrice[i] = true_close[i];
              pos = 1;
              placeholder_time_wait = 0;
            }
          }
        }
        placeholder_time_wait--;
      }
      
    } else { // pos != 0
      if (enter[i] == -2) { // exit
        ExitPrice[i] = close[i];
        ExitDate[i] = date[i];
        ExitTape[i] = "EXIT.MKT";
        pos = 0;
      }
    }
  }
  
  out["Enter.Price"] = EnterPrice;
  out["Enter.Date"] = EnterDate;
  out["Exit.Price"] = ExitPrice;
  out["Exit.Date"] = ExitDate;
  out["Enter.Tape"] = EnterTape;
  out["Exit.Tape"] = ExitTape;
  out["True.Price"] = TruePrice;
  
  return out;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
timesTwo(42)
*/
