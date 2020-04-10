// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
Rcpp::List CJSRL4fwdalg(const arma::mat& Y, const arma::cube& X, const arma::vec& beta, const double p, const double psi) {
    int n = Y.n_rows, J = Y.n_cols, pcov = X.n_slices; 
    int tau = 0;
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
      tau=0;
      for(int j=1;j<J;j++){
        Phi(0,0)=phi(j);
        Phi(1,0)=1-phi(j);
        if(arma::is_finite(Y(i,j))){
            if(Y(i,j)==0){  
              P(0,0)=1-p;
              P(1,1)=1-p*pow(psi,j-tau);
            }
            if(Y(i,j)==1){  
              P(0,0)=p;
              P(1,1)=0;
              tau=j;
            }
            if(Y(i,j)==2){  
              P(0,0)=0;
              P(1,1)=p*pow(psi,j-tau);
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
      } 
    }

    return Rcpp::List::create(Rcpp::Named("ll") = ll);
}
