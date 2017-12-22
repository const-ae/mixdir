
mixdir_vi_dp <- function(X, n_latent, alpha, beta, n_cat, max_iter, epsilon,
                      omega_init=NULL, zeta_init=NULL, phi_init=NULL, verbose=FALSE,
                      na.handle=c("mar", "category")){
  na.handle = match.arg(na.handle)
  if(na.handle != "mar") stop("Can only handle missing values under mar assumption")
  # Initialize the parameters
  n_ind <- nrow(X)
  n_quest <- ncol(X)

  if(length(alpha) == 0){
    alpha1 <- 1
    alpha2 <- 1
  }else if(length(alpha) == 1){
    alpha1 <- alpha[1]
    alpha2 <- alpha[1]
  }else if(length(alpha) == 2){
    alpha1 <- alpha[1]
    alpha2 <- alpha[2]
  }else{
    alpha1 <- alpha[1]
    alpha2 <- alpha[1]
  }

  if(is.null(omega_init)){
    omega_init <- rep(1, n_latent)
  }
  if(is.null(zeta_init)){
    zeta_init <- extraDistr::rdirichlet(n_ind, rep(1, n_latent))
  }
  if(is.null(phi_init)){
    phi_init <- array(sample(1:3, size=n_quest*n_latent*n_cat, replace=TRUE), dim=c(n_quest, n_latent, n_cat))
  }
  kappa1 <-omega_init
  kappa2 <-omega_init
  zeta <- zeta_init
  phi <- phi_init


  # Run the algorithm
  iter <- 1
  converged <- FALSE
  elbo_hist <- rep(NA, max_iter)
  while(iter <= max_iter & ! converged){
    # Update omega
    kappa1 <- alpha1 + colSums(zeta)
    ### q(z_i > t)
    summed_up_phi <- t(apply(zeta, 1, function(row) c(rev(cumsum(rev(row))), 0)))[, 2:(n_latent+1)]
    kappa2 <- alpha2 + colSums(summed_up_phi)

    # Update zeta
    for(k in 1:n_latent){
      zeta[, k] <- sapply(1:n_ind, function(i){
        exp(
          (digamma(kappa1[k]) - digamma(kappa1[k] + kappa2[k])) +
            (if(k > 1) sum(sapply(1:(k-1), function(kp) (digamma(kappa2[kp]) - digamma(kappa1[kp] + kappa2[kp])))) else 0) +
            sum(sapply(1:n_quest, function(j){
              if(is.na(X[i,j])){
                # If X_ij is missing replace it with sum over expected \E[X_ij == r] = phi[j,k,r]/sum(phi[j,k,])
                sum(sapply(1:n_cat, function(r) phi[j,k,r]/sum(phi[j,k,]) * (digamma(phi[ j, k, r]) - digamma(sum(phi[j, k, ])))))
              }else{
                digamma(phi[ j, k, X[i,j]]) - digamma(sum(phi[j, k, ]))
              }
            })) - 1
        )
      })
    }
    zeta <- zeta / rowSums(zeta)

    # Update phi
    for(j in 1:n_quest){
      for(r in 1:n_cat){
        phi[j, ,r] <- colSums(zeta * (X[, j] == r), na.rm=TRUE) + beta[r]
      }
    }

    elbo <- dp_expec_log_v(kappa1, kappa2, alpha1, alpha2) +
      sum(sapply(1:n_ind, function(i)dp_expec_log_zi(zeta[i, ], kappa1, kappa2))) +
      sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k) dp_expec_log_ujk(phi[j, k, ], beta) )))) +
      sum(sapply(1:n_ind, function(i) sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k)
        dp_expec_log_xij(X[i,j], phi[j, k, ], zeta[i,k]) )))))) +
      dp_entrop_v(kappa1, kappa2) +
      sum(sapply(1:n_ind, function(i)dp_entrop_zeta(zeta[i, ]))) +
      sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k) dp_entrop_phi(phi[j, k, ]) ))))


    if(iter != 1 && ! is.infinite(elbo) && abs(elbo - elbo_hist[iter - 1]) < epsilon) converged <- TRUE

    if(verbose && iter %% 10 == 0) message(paste0("Iter: ", iter, " ELBO: ", formatC(elbo, digits=8)))

    elbo_hist[iter] <- elbo
    iter <- iter + 1
  }


  elbo_hist <- elbo_hist[! is.na(elbo_hist)]

  U <- array(NA, dim=c(n_quest, n_latent, n_cat))
  for(j in 1:n_quest){
    for(k in 1:n_latent){
      U[j, k, ] <- phi[j,k, ] / sum(phi[j,k,])
    }
  }

  list(
    converged=converged,
    convergence=elbo_hist,
    lambda=colSums(zeta)/sum(colSums(zeta)),
    z=zeta,
    U=U,
    specific_params=list(
      kappa1=kappa1,
      kappa2=kappa2,
      phi=phi
    )
  )

}





# Helper Functions for VI


dp_expec_log_v <- function(kappa_a, kappa_b, alpha, beta){
  sum(sapply(1:(length(alpha)), function(k){
    (alpha[k] - 1) * (digamma(kappa_a[k]) - digamma(kappa_a[k] + kappa_b[k])) + (beta[k] - 1) * (digamma(kappa_b[k]) - digamma(kappa_a[k] + kappa_b[k]))
  }))
}

dp_expec_log_zi <- function(zeta_i, kappa_a, kappa_b){
  sum(sapply(1:length(zeta_i), function(k){
    if(k != length(zeta_i)){
      sum(zeta_i[(k+1):length(zeta_i)]) * (digamma(kappa_b[k]) - digamma(kappa_a[k] + kappa_b[k])) +
        zeta_i[k] * (digamma(kappa_a[k]) - digamma(kappa_a[k] + kappa_b[k]))
    }else {
      zeta_i[k] * (digamma(kappa_a[k]) - digamma(kappa_a[k] + kappa_b[k]))
    }
  }))
}

dp_expec_log_ujk <- function(phi_jk, beta){
  lgamma(sum(beta)) - sum(lgamma(beta)) + sum(sapply(1:length(beta), function(k)(beta[k] - 1) * (digamma(phi_jk[k]) - digamma(sum(phi_jk)))))
}

dp_expec_log_xij <- function(x_ij, phi_jk, zeta_ik){
  if(is.na(x_ij)){
    zeta_ik * sum(phi_jk/sum(phi_jk) * (digamma(phi_jk) - digamma(sum(phi_jk))))
  }else{
    zeta_ik * (digamma(phi_jk[x_ij]) - digamma(sum(phi_jk)))
  }
}

dp_entrop_v <- function(kappa_a, kappa_b){
  # Entropy of beta distributions
  sum(sapply(1:(length(kappa_a)), function(k){
    lgamma(kappa_a[k]) + lgamma(kappa_b[k]) - lgamma(kappa_a[k] + kappa_b[k]) -
      (kappa_a[k] - 1) * digamma(kappa_a[k]) - (kappa_b[k] - 1) * digamma(kappa_b[k]) +
      (kappa_a[k] + kappa_b[k] - 2) * digamma(kappa_a[k] + kappa_b[k])
  }))
}

dp_entrop_zeta <- function(zeta_i){
  zeta_i[zeta_i <= .Machine$double.xmin] <- .Machine$double.xmin
  - sum(zeta_i * log(zeta_i))
}

dp_entrop_phi <- function(phi_jk){
  - lgamma(sum(phi_jk)) + sum(lgamma(phi_jk)) - sum(sapply(1:length(phi_jk), function(k)(phi_jk[k] - 1) * (digamma(phi_jk[k]) - digamma(sum(phi_jk)))))
}



