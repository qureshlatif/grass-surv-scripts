#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
Rcpp::List CJSRL5fwdalg(const arma::mat& Y, const arma::cube& X, const arma::vec& beta, const double p, const double psi, const arma::mat& FL) {
    int n = Y.n_rows, J = Y.n_cols, pcov = X.n_slices; 
    double ll=0;
    arma::colvec theta=arma::zeros(2);
    arma::colvec v=arma::zeros(2);
    arma::mat P=arma::zeros(2,2);    
    arma::mat Phi=arma::zeros(2,2); 
    arma::mat Xrow(J,pcov); 

    Phi(0,1)=0;
    Phi(1,1)=1;

    for(int i=0;i<n;i++){
      Xrow=X.row(i);
      arma::colvec logit=Xrow*beta;
      arma::colvec phi=exp(logit)/(1+exp(logit));
      theta(0)=1;
      theta(1)=0;
      for(int j=FL(i,0);j<J;j++){
        Phi(0,0)=phi(j);
        Phi(1,0)=1-phi(j);
        if(arma::is_finite(Y(i,j))){
            if(Y(i,j)==0){  
              P(0,0)=1-p;
              P(1,1)=1-psi;
            }
            if(Y(i,j)==1){  
              P(0,0)=p;
              P(1,1)=0;
            }
            if(Y(i,j)==2){  
              P(0,0)=0;
              P(1,1)=psi;
            }
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
// Rcpp::Rcout << ll << std::endl;

      } 
    }

    return Rcpp::List::create(Rcpp::Named("ll") = ll);
}
