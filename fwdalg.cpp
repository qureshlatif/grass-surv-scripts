// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
Rcpp::List fwdalg(const arma::mat& Y, const arma::cube& X, const arma::vec& beta, const double pdetect) {
    int n = Y.n_rows, J = Y.n_cols, p = X.n_slices;
    double ll=0;
    arma::colvec theta=arma::zeros(2);
    arma::colvec v=arma::zeros(2);
    arma::mat P=arma::zeros(2,2);    
    arma::mat Phi=arma::zeros(2,2); 
    arma::mat Xrow(J,p); 

    Phi(0,0)=1;

    for(int i=0;i<n;i++){
      Xrow=X.row(i);
      arma::colvec logit=Xrow*beta;
      arma::colvec phi=exp(logit)/(1+exp(logit));
      theta(0)=0;
      theta(1)=1;
      for(int j=1;j<J;j++){
        Phi(0,1)=1-phi(j);
        Phi(1,1)=phi(j);
        if(arma::is_finite(Y(i,j))){
          P(0,0)=1-Y(i,j);
          P(1,1)=pow(pdetect,Y(i,j))*pow(1-pdetect,1-Y(i,j));
        }
        if(!arma::is_finite(Y(i,j))){
          P(0,0)=1;
          P(1,1)=1;
        }
        arma::vec v=P*Phi*theta;
        double u=arma::accu(v);
        ll=ll+log(u);
        theta=v/u;
// Rcpp::Rcout << i << " " << j << std::endl;
// Rcpp::Rcout << Y(i,j) << std::endl;
// Rcpp::Rcout << P << std::endl;
// Rcpp::Rcout << Phi << std::endl;
// Rcpp::Rcout << theta << std::endl;
      } 
    }

    return Rcpp::List::create(Rcpp::Named("ll") = ll);
}
