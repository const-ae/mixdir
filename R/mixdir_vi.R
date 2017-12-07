
mixdir_vi <- function(X, n_latent, alpha, beta, n_cat, max_iter, epsilon,
                      omega_init=NULL, zeta_init=NULL, phi_init=NULL, verbose=FALSE){
  # Initialize the parameters
  n_ind <- nrow(X)
  n_quest <- ncol(X)
  if(is.null(omega_init)){
    omega_init <- rep(1, n_latent)
  }
  if(is.null(zeta_init)){
    zeta_init <- extraDistr::rdirichlet(n_ind, rep(1, n_latent))
  }
  if(is.null(phi_init)){
    phi_init <- array(sample(1:3, size=n_quest*n_latent*n_cat, replace=TRUE), dim=c(n_quest, n_latent, n_cat))
  }
  omega <-omega_init
  zeta <- zeta_init
  phi <- phi_init

  # Run the algorithm
  iter <- 1
  converged <- FALSE
  elbo_hist <- rep(NA, max_iter)
  while(iter <= max_iter & ! converged){

    # Update omega
    omega <- alpha + colSums(zeta)

    # Update zeta
    for(k in 1:n_latent){
      zeta[, k] <- sapply(1:n_ind, function(i){
        # exp(digamma(omega[k]) - digamma(sum(omega)) + sum(digamma(phi[,k, X[i, ]]) - rowSums(digamma(phi[,k, ]))) - 1)
        exp(digamma(omega[k]) - digamma(sum(omega)) + sum(sapply(1:n_quest, function(j) digamma(phi[ j, k, X[i,j]]) - digamma(sum(phi[j, k, ])))) - 1)
      })
    }
    zeta <- zeta / rowSums(zeta)

    # Update phi
    for(j in 1:n_quest){
      for(r in 1:n_cat){
        phi[j, ,r] <- colSums(zeta * (X[, j] == r)) + beta[r]
      }
    }

    elbo <- expec_log_lambda(omega, alpha) +
      sum(sapply(1:n_ind, function(i)expec_log_zi(zeta[i, ], omega))) +
      sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k) expec_log_ujk(phi[j, k, ], beta) )))) +
      sum(sapply(1:n_ind, function(i) sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k) expec_log_xij(X[i,j], phi[j, k, ], zeta[i,k]) )))))) +
      entrop_omega(omega) +
      sum(sapply(1:n_ind, function(i)entrop_zeta(zeta[i, ]))) +
      sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k) entrop_phi(phi[j, k, ]) ))))

    if(iter != 1 && ! is.infinite(elbo) && elbo - elbo_hist[iter - 1] < epsilon) converged <- TRUE

    if(verbose && iter %% 10 == 0) message(paste0("Iter: ", iter, " ELBO: ", formatC(elbo, digits=4)))

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
    lambda=omega/sum(omega),
    z=zeta,
    U=U,
    specific_params=list(
      omega=omega,
      phi=phi
    )
  )

}





# Helper Functions for VI

expec_log_lambda <- function(omega, alpha) {
  lgamma(sum(alpha)) - sum(lgamma(alpha)) + sum(sapply(1:length(alpha), function(k)(alpha[k] - 1) * (digamma(omega[k]) - digamma(sum(omega)))))
}

expec_log_zi <- function(zeta_i, omega){
  sum(zeta_i * (digamma(omega) - digamma(sum(omega))))
}

expec_log_ujk <- function(phi_jk, beta){
  expec_log_lambda(phi_jk, beta)
}

expec_log_xij <- function(x_ij, phi_jk, zeta_ik){
  zeta_ik * (digamma(phi_jk[x_ij]) - digamma(sum(phi_jk)))
}

entrop_omega <- function(omega){
  - lgamma(sum(omega)) + sum(lgamma(omega)) - sum(sapply(1:length(omega), function(k)(omega[k] - 1) * (digamma(omega[k]) - digamma(sum(omega)))))
}

entrop_zeta <- function(zeta_i){
  zeta_i[zeta_i <= .Machine$double.xmin] <- .Machine$double.xmin
  - sum(zeta_i * log(zeta_i))
}

entrop_phi <- function(phi_jk){
  entrop_omega(phi_jk)
}



