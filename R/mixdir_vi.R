
mixdir_vi <- function(X, n_latent, alpha, beta, categories, max_iter, epsilon,
                      omega_init=NULL, zeta_init=NULL, phi_init=NULL, verbose=FALSE){

  # Initialize the parameters
  n_ind <- nrow(X)
  n_quest <- ncol(X)
  n_cat <- max(X, na.rm=TRUE)

  if(is.null(alpha) || length(alpha) == 0){
    alpha <- 1
  }else if(length(alpha) == 1){
    alpha <- alpha[1]
  }else{
    warning(paste0("alpha should only be a single value. Using alpha=alpha[1]=", alpha[1]))
    alpha <- alpha[1]
  }

  if(is.null(beta) || length(beta) == 0){
    beta <- 0.1
  }else if(length(beta) == 1){
    beta <- beta[1]
  }else{
    warning(paste0("beta should only be a single value. Using beta=beta[1]=", beta[1]))
    beta <- beta[1]
  }

  if(is.null(omega_init)){
    omega_init <- rep(1, n_latent)
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

  omega <-omega_init
  zeta <- zeta_init
  phi <- phi_init
  phia <- conv_phi_to_array(phi, n_quest, n_latent)
  # Run the algorithm
  iter <- 1
  converged <- FALSE
  elbo_hist <- rep(NA, max_iter)
  while(iter <= max_iter & ! converged){

    # Update omega
    omega <- alpha + colSums(zeta)

    # Update zeta
    # for(k in 1:n_latent){
    #   zeta[, k] <- sapply(1:n_ind, function(i){
    #     exp(digamma(omega[k]) - digamma(sum(omega)) + sum(sapply(1:n_quest, function(j){
    #       if(is.na(X[i,j])){
    #         # If X_ij is missing ignore it
    #         0
    #       }else{
    #         digamma(phi[[j]][[k]][X[i,j]]) - digamma(sum(phi[[j]][[k]]))
    #       }
    #     })) - 1)
    #   })
    # }
    zeta <- update_zeta_cpp(zeta, X, phia, omega, n_ind, n_quest, n_latent, n_cat)
    zeta <- zeta / rowSums(zeta)

    # Update phi
    for(j in 1:n_quest){
      for(k in 1:n_latent){
        for(r in seq_along(categories[[j]])){
          phi[[j]][[k]][r] <- sum(zeta[ ,k] * (X[, j] == r), na.rm=TRUE) + beta
        }
      }
    }
    phia <- conv_phi_to_array(phi, n_quest, n_latent)

    elbo <- expec_log_lambda(omega, rep(alpha, length(omega))) +
      sum(sapply(1:n_ind, function(i)expec_log_zi(zeta[i, ], omega))) +
      sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k) expec_log_ujk(phi[[j]][[k]], rep(beta, length(categories[[j]]))) )))) +
      # sum(sapply(1:n_ind, function(i) sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k)
      #   expec_log_xij(X[i,j], phi[[j]][[k]], zeta[i,k]) )))))) +
      expec_log_x_cpp(X, phia, zeta, n_quest, n_latent, n_cat) +
      entrop_omega(omega) +
      sum(sapply(1:n_ind, function(i)entrop_zeta(zeta[i, ]))) +
      sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k) entrop_phi(phi[[j]][[k]]) ))))


    if(iter != 1 && ! is.infinite(elbo) && elbo - elbo_hist[iter - 1] < - epsilon){
      warning(paste0("The ELBO decreased. This should not happen, it might be due to numerical instabilities or a bug in the code. ",
                     "Please contact the maintainer to report this.\n"))
    }
    if(iter != 1 && ! is.infinite(elbo) && elbo - elbo_hist[iter - 1] < epsilon) converged <- TRUE

    if(verbose && iter %% 10 == 0) message(paste0("Iter: ", iter, " ELBO: ", formatC(elbo, digits=4)))

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

  omega <- alpha + colSums(zeta)
  lambda <- omega/sum(omega)

  # Bring the zeta and U to a consistent state (lambda is slightly off)
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
  lgamma(sum(beta)) - sum(lgamma(beta)) + sum(sapply(1:length(beta), function(k)(beta[k] - 1) * (digamma(phi_jk[k]) - digamma(sum(phi_jk)))))
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

entrop_phi <- function(phi_jk){
  - lgamma(sum(phi_jk)) + sum(lgamma(phi_jk)) - sum(sapply(1:length(phi_jk), function(k)(phi_jk[k] - 1) * (digamma(phi_jk[k]) - digamma(sum(phi_jk)))))
}



conv_phi_to_array <- function(phi, n_quest, n_latent){
  cat_length <- sapply(phi, function(phi_j) length(phi_j[[1]]))
  phia <- array(NA_real_, dim = c(n_quest, n_latent, max(cat_length)))
  for(j in 1:n_quest){
    for(k in 1:n_latent){
      for(r in 1:cat_length[j]){
        phia[j,k,r] <- phi[[j]][[k]][r]
      }
    }
  }
  phia
}
