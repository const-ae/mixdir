

library(mixdir)


library(Rcpp)
cppFunction('double rcppCalc(NumericMatrix X, NumericVector phia, NumericMatrix zeta, int n_quest, int n_latent, int n_cat){
  double result = 0;
  for(int j = 0; j < X.ncol(); j++){
    for(int k = 0; k < 3; k++){

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
}')




rearranged_for_loop3 <- function(X, phia, zeta){
  counter <- 0
  for(j in 1:ncol(X)){
    for(k in 1:3){
      dg_sum <- digamma(sum(phia[j,k,], na.rm=TRUE))
      for(i in 1:nrow(X)){
        if(is.na(X[i,j])){
          counter <- counter + 1
        }else{
          counter <- counter + zeta[i,k] * (digamma(phia[j,k, X[i,j]]) - dg_sum)
        }
      }
    }
  }
  counter
}

rearranged_for_loop2 <- function(X, phia, zeta){
  counter <- 0
  for(i in 1:nrow(X)){
    for(j in 1:ncol(X)){
      if(is.na(X[i,j])){
        counter <- counter + 3
      }else{
        for(k in 1:3){
          counter <- counter + zeta[i,k] * (digamma(phia[j,k, X[i,j]]) - digamma(sum(phia[j,k,], na.rm=TRUE)))
        }
      }
    }
  }
  counter
}

orig_for_loop2 <- function(X, phia, zeta){
  counter <- 0
  for(i in 1:nrow(X)){
    for(j in 1:ncol(X)){
      for(k in 1:3){
        if(is.na(X[i,j])){
          counter <- counter + 1
        }else{
          counter <- counter + zeta[i,k] * (digamma(phia[j,k, X[i,j]]) - digamma(sum(phia[j,k,], na.rm=TRUE)))
        }
      }
    }
  }
  counter
}

vect_with_for_loop2 <- function(X, phia, zeta){
  counter <- 0
  zeta_vec <- array(NA_real_, dim=c(nrow(X),ncol(X), 3))
  phi_vec <- array(NA_real_, dim=c(nrow(X),ncol(X), 3))
  sum_phi_vec <- array(NA_real_, dim=c(nrow(X),ncol(X), 3))
  na_vec <-array(NA_real_, dim=c(nrow(X),ncol(X), 3))
  for(i in 1:nrow(X)){
    for(j in 1:ncol(X)){
      for(k in 1:3){
        na_vec[i,j, k] <- (is.na(X[i,j])) * 1.0
        zeta_vec[i,j,k] <- zeta[i,k]
        phi_vec[i,j,k] <- if(is.na(X[i,j])) 1 else phia[j,k, X[i,j]]
        sum_phi_vec[i,j,k] <- if(is.na(X[i,j])) 1 else sum(phia[j,k,], na.rm=TRUE)
      }
    }
  }
  sum(as.numeric(zeta_vec) * (digamma(as.numeric(phi_vec)) - digamma(as.numeric(sum_phi_vec))) * (1-na_vec) + na_vec)
}

vect_with_for_loop <- function(X, phi, zeta){
  counter <- 0
  zeta_vec <- array(NA_real_, dim=c(nrow(X),ncol(X), 3))
  phi_vec <- array(NA_real_, dim=c(nrow(X),ncol(X), 3))
  sum_phi_vec <- array(NA_real_, dim=c(nrow(X),ncol(X), 3))
  na_vec <-array(NA_real_, dim=c(nrow(X),ncol(X), 3))
  for(i in 1:nrow(X)){
    for(j in 1:ncol(X)){
      for(k in 1:3){
        na_vec[i,j, k] <- (is.na(X[i,j])) * 1.0
        zeta_vec[i,j,k] <- zeta[i,k]
        phi_vec[i,j,k] <- if(is.na(X[i,j])) 1 else phi[[j]][[k]][X[i,j]]
        sum_phi_vec[i,j,k] <- if(is.na(X[i,j])) 1 else sum(phi[[j]][[k]])
      }
    }
  }

  sum(as.numeric(zeta_vec) * (digamma(as.numeric(phi_vec)) - digamma(as.numeric(sum_phi_vec))))
}


orig_for_loop <- function(X, phi, zeta){
  counter <- 0
  for(i in 1:nrow(X)){
    for(j in 1:ncol(X)){
      for(k in 1:3){
        if(is.na(X[i,j])){
          counter <- counter + 1
        }else{
          counter <- counter + zeta[i,k] * (digamma(phi[[j]][[k]][X[i,j]]) - digamma(sum(phi[[j]][[k]])))
        }
      }
    }
  }
  counter
}

orig_call_inlined <- function(X, phi, zeta){
  sum(sapply(1:nrow(X), function(i) sum(sapply(1:ncol(X), function(j) sum(sapply(1:3, function(k){
    if(is.na(X[i,j])){
      1
    }else{
      zeta[i,k] * (digamma(phi[[j]][[k]][X[i,j]]) - digamma(sum(phi[[j]][[k]])))
    }
  }))))))
}





context("ELBO Calculation")
test_that("Find out how to optimize the functions", {
  data("mushroom")
  X <- mushroom[sample(1:nrow(mushroom), 100), 1:5]
  X2 <- as.matrix(purrr::map_df(purrr::map_df(X, as.factor), as.numeric))
  any(is.na(X2))
  set.seed(1)
  result <- mixdir::mixdir(X, n_latent=3, select_latent=FALSE, max_iter = 2)

  phi <- result$specific_params$phi
  zeta <- result$class_prob
  phia <- conv_phi_to_array(phi, ncol(X), 3)



  microbenchmark::microbenchmark(orig_for_loop2(X2, phia, zeta),
                                 rearranged_for_loop2(X2, phia, zeta),
                                 rearranged_for_loop3(X2, phia, zeta),
                                 orig_call_inlined(X, phi, zeta),
                                 rcppCalc(X2, as.numeric(phia), zeta, dim(phia)[1], dim(phia)[2], dim(phia)[3]),
                                 times=30)



})


































