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
double expec_log_x_cpp(NumericMatrix X, NumericVector phia, NumericMatrix zeta, int n_quest, int n_latent, int n_cat){
  double result = 0;
  for(int j = 0; j < X.ncol(); j++){
    for(int k = 0; k < n_latent; k++){

      double dg_sum = 0;
      for(int r = 0; r < n_cat; r++){
        if(! NumericVector::is_na(phia[j + k * n_quest + r * n_quest * n_latent])){
          dg_sum += phia[j + k * n_quest + r * n_quest * n_latent];
        }
      }
      dg_sum = R::digamma(dg_sum);

      for(int i = 0; i < X.nrow(); i++){
        if(NumericMatrix::is_na(X(i,j))){
          result += 1;
        }else{
          result += zeta(i,k) * (R::digamma(phia[j  + k * n_quest + (X(i,j)-1) * n_quest * n_latent]) - dg_sum);
        }
      }
    }
  }
  return result;
}


/*
 * This was implemented in C++ to speed up the computation
 *
 *      for(k in 1:n_latent){
 *        zeta[, k] <- sapply(1:n_ind, function(i){
 *          exp(digamma(omega[k]) - digamma(sum(omega)) + sum(sapply(1:n_quest, function(j){
 *            if(is.na(X[i,j])){
 *              # If X_ij is missing ignore it
 *              0
 *            }else{
 *              digamma(phi[[j]][[k]][X[i,j]]) - digamma(sum(phi[[j]][[k]]))
 *            }
 *          })) - 1)
 *        })
 *      }
 */
// [[Rcpp::export]]
NumericMatrix update_zeta_cpp(NumericMatrix zeta, NumericMatrix X, NumericVector phia, NumericVector omega, int n_ind, int n_quest, int n_latent, int n_cat){
  double dg_sum_omega = R::digamma(sum(omega));

  NumericVector dg_phi_sums(n_quest * n_latent);
  for(int k = 0; k < n_latent; k++){
    for(int j = 0; j < n_quest; j++){
      double dg_sum = 0;
      for(int r = 0; r < n_cat; r++){
        if(! NumericVector::is_na(phia[j + k * n_quest + r * n_quest * n_latent])){
          dg_sum += phia[j + k * n_quest + r * n_quest * n_latent];
        }
      }
      dg_phi_sums[j + k * n_quest] = R::digamma(dg_sum);
    }
  }


  for(int k = 0; k < n_latent; k++){
    for(int i = 0; i < n_ind; i++){
      double sum_dg_phi = 0;
      for(int j = 0; j < n_quest; j++){
        if(! NumericMatrix::is_na(X(i,j))){
          sum_dg_phi += R::digamma(phia[j  + k * n_quest + (X(i,j)-1) * n_quest * n_latent]) - dg_phi_sums[j + k * n_quest];
        }
      }
      zeta(i,k) = exp(R::digamma(omega[k]) - dg_sum_omega + sum_dg_phi - 1);
    }
  }
  return zeta;
}


/*
 *  This was implemented in C++ to speed up the computation
 *
 *     for(k in 1:n_latent){
 *       zeta[, k] <- sapply(1:n_ind, function(i){
 *         exp(
 *           (digamma(kappa1[k]) - digamma(kappa1[k] + kappa2[k])) +
 *             (if(k > 1) sum(sapply(1:(k-1), function(kp) (digamma(kappa2[kp]) - digamma(kappa1[kp] + kappa2[kp])))) else 0) +
 *             sum(sapply(1:n_quest, function(j){
 *               if(is.na(X[i,j])){
 *                 # If X_ij is missing ignore it
 *                 0
 *               }else{
 *                  digamma(phi[[j]][[k]][X[i,j]]) - digamma(sum(phi[[j]][[k]]))
 *               }
 *             })) - 1
 *         )
 *       })
 *     }
 *
 *
 *
 */
// [[Rcpp::export]]
NumericMatrix update_zeta_dp_cpp(NumericMatrix zeta, NumericMatrix X, NumericVector phia,
                                 NumericVector kappa1, NumericVector kappa2,
                                 int n_ind, int n_quest, int n_latent, int n_cat){

  NumericVector dg_phi_sums(n_quest * n_latent);
  for(int k = 0; k < n_latent; k++){
    for(int j = 0; j < n_quest; j++){
      double dg_sum = 0;
      for(int r = 0; r < n_cat; r++){
        if(! NumericVector::is_na(phia[j + k * n_quest + r * n_quest * n_latent])){
          dg_sum += phia[j + k * n_quest + r * n_quest * n_latent];
        }
      }
      dg_phi_sums[j + k * n_quest] = R::digamma(dg_sum);
    }
  }

  NumericVector sum_dp_kps(n_latent);
  for(int k = 0; k < n_latent; k++){
    double sum_dg_kp = 0;
    for(int kp = 0; kp < k ; kp++){
      sum_dg_kp += R::digamma(kappa2[kp]) - R::digamma(kappa1[kp] + kappa2[kp]);
    }
    sum_dp_kps[k] = sum_dg_kp;
  }

  for(int k = 0; k < n_latent; k++){
    for(int i = 0; i < n_ind; i++){
      double sum_dg_phi = 0;
      for(int j = 0; j < n_quest; j++){
        if(! NumericMatrix::is_na(X(i,j))){
          sum_dg_phi += R::digamma(phia[j  + k * n_quest + (X(i,j)-1) * n_quest * n_latent]) - dg_phi_sums[j + k * n_quest];
        }
      }
      zeta(i,k) = exp(R::digamma(kappa1[k]) - R::digamma(kappa1[k] + kappa2[k]) + sum_dp_kps[k] + sum_dg_phi - 1);
    }
  }
  return zeta;
}

