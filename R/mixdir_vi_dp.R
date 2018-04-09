
mixdir_vi_dp <- function(X, n_latent, alpha, beta, categories, max_iter, epsilon,
                         kappa1_init=NULL, kappa2_init=NULL, zeta_init=NULL, phi_init=NULL, verbose=FALSE){

  # Initialize the parameters
  n_ind <- nrow(X)
  n_quest <- ncol(X)
  n_cat <- max(X, na.rm=TRUE)

  if(is.null(alpha) || length(alpha) == 0){
    alpha1 <- 1
    alpha2 <- 1
  }else if(length(alpha) == 2){
    alpha1 <- alpha[1]
    alpha2 <- alpha[2]
  }else{
    warning(paste0("alpha shoul only be a vector of 2 values. Using alpha1=alpha2=alpha[1]=", alpha[1]))
    alpha1 <- alpha[1]
    alpha2 <- alpha[1]
  }

  if(is.null(beta) || length(beta) == 0){
    beta <- 0.1
  }else if(length(beta) == 1){
    beta <- beta[1]
  }else{
    warning(paste0("beta should only be a single value. Using beta=beta[1]=", beta[1]))
    beta <- beta[1]
  }

  if(is.null(kappa1_init)){
    kappa1_init <- rep(1, n_latent)
  }
  if(is.null(kappa2_init)){
    kappa2_init <- rep(1, n_latent)
  }
  if(is.null(zeta_init)){
    zeta_init <- extraDistr::rdirichlet(n_ind, rep(1, n_latent))
  }
  if(is.null(phi_init)){
    phi_init <- lapply(1:n_quest, function(j) lapply(1:n_latent, function(k) {
      x <- sample(1:3, size=length(categories[[j]]), replace=TRUE)
      # names(x) <- categories[[j]]
      x
    }))
  }else{
    for(k in 1:n_latent){
      phi_init_elem_lengths <- sapply(phi_init, function(x) length(x[[k]]))
      if(! all(phi_init_elem_lengths == sapply(categories, length))){
        stop(paste0("phi_init has the wrong number of elements for feature: ",
                    paste0(which(phi_init_elem_lengths != sapply(categories, length)), collapse = ", ")))
      }
    }
  }

  kappa1 <-kappa1_init
  kappa2 <-kappa2_init
  zeta <- zeta_init
  phi <- phi_init
  phia <- conv_phi_to_array(phi, n_quest, n_latent)

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
    # for(k in 1:n_latent){
    #   zeta[, k] <- sapply(1:n_ind, function(i){
    #     exp(
    #       (digamma(kappa1[k]) - digamma(kappa1[k] + kappa2[k])) +
    #         (if(k > 1) sum(sapply(1:(k-1), function(kp) (digamma(kappa2[kp]) - digamma(kappa1[kp] + kappa2[kp])))) else 0) +
    #         sum(sapply(1:n_quest, function(j){
    #           if(is.na(X[i,j])){
    #             # If X_ij is missing ignore it
    #             0
    #           }else{
    #             digamma(phi[[j]][[k]][X[i,j]]) - digamma(sum(phi[[j]][[k]]))
    #           }
    #         })) - 1
    #     )
    #   })
    # }
    zeta <- update_zeta_dp_cpp(zeta, X, phia, kappa1, kappa2, n_ind, n_quest, n_latent, n_cat)
    zeta <- zeta / rowSums(zeta)
    zeta <- zeta[, order(-colSums(zeta))]       # Make sure the zeta columns are ordered optimally


    # Update phi
    for(j in 1:n_quest){
      for(k in 1:n_latent){
        for(r in seq_along(categories[[j]])){
          phi[[j]][[k]][r] <- sum(zeta[ ,k] * (X[, j] == r), na.rm=TRUE) + beta
        }
      }
    }
    phia <- conv_phi_to_array(phi, n_quest, n_latent)

    elbo <- dp_expec_log_v(kappa1, kappa2, rep(alpha1, length(kappa1)), rep(alpha2, length(kappa2))) +
      sum(sapply(1:n_ind, function(i)dp_expec_log_zi(zeta[i, ], kappa1, kappa2))) +
      sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k) expec_log_ujk(phi[[j]][[k]], rep(beta, length(categories[[j]]))) )))) +
      # sum(sapply(1:n_ind, function(i) sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k)
      #   dp_expec_log_xij(X[i,j], phi[[j]][[k]], zeta[i,k]) )))))) +
      expec_log_x_cpp(X, phia, zeta, n_quest, n_latent, n_cat) +
      dp_entrop_v(kappa1, kappa2) +
      sum(sapply(1:n_ind, function(i) entrop_zeta(zeta[i, ]))) +
      sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k) entrop_phi(phi[[j]][[k]]) ))))

    if(iter != 1 && ! is.infinite(elbo) && elbo - elbo_hist[iter - 1] < - epsilon)
      warning(paste0("The ELBO decreased. This should not happen, it might be due to numerical instabilities or a bug in the code. ",
                     "Please contact the maintainer to report this.\n"))
    if(iter != 1 && ! is.infinite(elbo) && elbo - elbo_hist[iter - 1] < epsilon) converged <- TRUE

    if(verbose && iter %% 10 == 0) message(paste0("Iter: ", iter, " ELBO: ", formatC(elbo, digits=8)))

    elbo_hist[iter] <- elbo
    iter <- iter + 1
  }


  elbo_hist <- elbo_hist[! is.na(elbo_hist)]

  U <- lapply(1:n_quest, function(j) lapply(1:n_latent, function(k) {
    x <- rep(NA, times=length(categories[[j]]))
    names(x) <- categories[[j]]
    x
  }))
  names(U) <- colnames(X)
  for(j in 1:n_quest){
    for(k in 1:n_latent){
      U[[j]][[k]] <- (phi[[j]][[k]]) / sum(phi[[j]][[k]])
      names(U[[j]][[k]]) <- categories[[j]]
    }
  }

  # A last update of omega
  kappa1 <- alpha1 + colSums(zeta)
  ### q(z_i > t)
  summed_up_phi <- t(apply(zeta, 1, function(row) c(rev(cumsum(rev(row))), 0)))[, 2:(n_latent+1)]
  kappa2 <- alpha2 + colSums(summed_up_phi)

  # Calculate lambda
  nu <- kappa1/(kappa1 + kappa2)
  lambda <- sapply(1:n_latent, function(k){
    nu[k] * (if(k > 1) prod((1-nu[1:(k-1)])) else 1)
  })

  if(lambda[n_latent] > 0.01){
    warning("The model put considerable weight on the last component.
            Consider re-running the model with an increased number of latent categories.")
  }

  # Calculate consistent prob_z
  prob_z <- matrix(vapply(seq_along(lambda), function(k){
    lambda[k] * exp(rowSums(log(matrix(vapply(colnames(X), function(j){
      ifelse(is.na(X[ ,j]), 1, U[[j]][[k]][X[, j]])
    }, FUN.VALUE=rep(0.0, times=n_ind)), nrow=n_ind))))
  }, FUN.VALUE=rep(0.0, times=n_ind)), nrow=n_ind)
  prob_z <- prob_z / rowSums(prob_z)

  list(
    converged=converged,
    convergence=elbo_hist,
    ELBO=elbo,
    lambda=lambda,
    pred_class=apply(prob_z, 1, which.max),
    class_prob=prob_z,
    category_prob=U,
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


dp_entrop_v <- function(kappa_a, kappa_b){
  # Entropy of beta distributions
  sum(sapply(1:(length(kappa_a)), function(k){
    lgamma(kappa_a[k]) + lgamma(kappa_b[k]) - lgamma(kappa_a[k] + kappa_b[k]) -
      (kappa_a[k] - 1) * digamma(kappa_a[k]) - (kappa_b[k] - 1) * digamma(kappa_b[k]) +
      (kappa_a[k] + kappa_b[k] - 2) * digamma(kappa_a[k] + kappa_b[k])
  }))
}





