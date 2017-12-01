



mixdir_mcmc <-  function(X, n_latent, alpha_a, alpha_b, beta, n_cat, max_iter, epsilon,
                         omega_init=NULL, zeta_init=NULL, phi_init=NULL, verbose=FALSE){

  n_ind <- nrow(X)
  n_quest <- ncol(X)

  pi_true <- array(0, sapply(1:n_quest, function(j)n_cat))
  for(id in 1:nrow(X)){
    pi_true[matrix(X[id, ], nrow=1)] <- pi_true[matrix(X[id, ], nrow=1)] + 1
  }
  pi_true <- pi_true/n_ind


  alpha <- rgamma(1, shape=1, rate=1)
  draw_latent_class <- function(n, alpha=1, max_class=1e3){
    V <- rbeta(max_class, 1, alpha)
    w <- c(V[1], rep(NA,max_class-1))
    w[2:max_class] <- sapply(2:max_class, function(i) V[i] * prod(1 - V[1:(i-1)]))
    list(class=factor(sample(1:max_class, n, prob=w, replace=TRUE), levels=1:max_class, ordered = TRUE),
         weights=w,
         V=V)
  }

  latent_classes <- draw_latent_class(n_ind)
  class <- latent_classes$class
  weights <- latent_classes$weights
  V <- latent_classes$V


  pi_est_hist <- as.list(rep(NA, max_iter))
  k_history <- as.list(rep(NA, max_iter))
  psi_history<- as.list(rep(NA, max_iter))
  weight_history <- as.list(rep(NA, max_iter))
  sse_hist <- rep(NA, max_iter)
  iter <- 1
  while(iter <= max_iter){

    # Update u_i ~ runif(0, nu_zi)
    u <- runif(n_ind, 0, weights[class])

    # Update psi_jh
    k <- as.numeric(max(class))
    psi <- as.list(rep(NA, 3))
    for(h in 1:(k+50)){
      for(j in 1:n_quest){
        counter <- c(A=0,B=0,C=0)
        for(i in 1:n_ind){
          # Overall all individuals for which latent class is h count number of A,B,C's
          if(class[i] == h){
            counter[X[i,j]] <- counter[X[i,j]] + 1
          }
        }
        psi_jh <- extraDistr::rdirichlet(1, beta[1] + counter)
        psi[[j]] <- if(all(is.na(psi[[j]]))) t(psi_jh) else cbind(psi[[j]], t(psi_jh))
      }
    }


    # Update V_h ~ beta(1, alpha), but needs to be in some interval
    for(h in 1:k){
      lower_bound <- max(c(0,sapply(1:n_ind, function(i){
        if(class[i] == h && h > 1) u[i]/(prod(1-V[1:(h-1)]))
        else if(class[i] == h) u[i]     # Product over empty set is 1
        else NA
      })), na.rm=TRUE)
      upper_bound <- 1 - max(c(0,sapply(1:n_ind, function(i){
        if(class[i] > h && class[i] > 1 && !(class[i] == 2 && h == 1)) {
          u[i]/(V[class[i]] * prod(1-V[setdiff(1:(as.numeric(class[i])-1), h)]))
        }else if(class[i] > h){
          u[i]/V[class[i]]            # Product over empty set is 1
        }else{
          NA
        }
      })), na.rm=TRUE)
      if(upper_bound < lower_bound) {
        stop(paste0("Upper bound (", upper_bound,
                    ") must not be smaller than lower bound (",
                    lower_bound, "), iter ", iter))
      }
      # Trick to sample from truncated beta
      support <- ppoints(100) * (upper_bound-lower_bound) + lower_bound
      V[h] <- sample(support, 1, prob=dbeta(support, 1, alpha))
    }

    # Update weights
    weights <- c(V[1], rep(NA,length(weights)-1))
    weights[2:length(weights)] <- sapply(2:length(weights), function(i) V[i] * prod(1 - V[1:(i-1)]))

    # Find \tilde{k}
    k_tilde <- min(which(cumsum(weights) > 1-min(u)))

    # Update z_i ~ multinomial
    for(i in 1:n_ind){
      h_probs <- sapply(1:k_tilde, function(h){
        if(weights[h] > u[i]){
          prod(sapply(1:n_quest, function(j)psi[[j]][X[i,j],h]))
        }else{
          0
        }
      })
      if(all(h_probs == 0)) {
        warning("All h_probs are 0")
        h_probs <- rep(1, k_tilde)
      }
      h_probs <- h_probs/sum(h_probs)
      class[i] <- sample(1:k_tilde, 1, prob=h_probs)
    }
    k <- as.numeric(max(class))

    # Update alpha ~ gamma
    alpha <- rgamma(1, shape=alpha_a + k,
                    rate=alpha_b - sum(log(1-V[1:k])))

    pi_est <- array(0, sapply(1:n_quest, function(j)nrow(psi[[j]])))
    for(i in purrr::cross_n(lapply(1:n_quest, function(r) seq(from=1, to=3)))){
      pi_est[matrix(unlist(i), nrow=1)] <- sum(sapply(1:k, function(h) weights[h] *
                                                        prod(sapply(1:n_quest, function(j) psi[[j]][i[[j]], h]))))
    }
    sse <- sum((pi_true - pi_est)^2)

    if(verbose && iter %% 10 == 0) print(paste0("Iter: ", iter, ", error: ", sse))
    sse_hist[iter] <- sse
    pi_est_hist[[iter]] <- pi_est
    k_history[[iter]] <- k
    psi_history[[iter]] <- lapply(psi, function(p)p[, 1:k, drop=FALSE])
    weight_history[[iter]] <- weights[1:k]

    iter <- iter + 1
  }

  after_burnin <- round(max_iter/2):max_iter

  U <- array(NA, dim=c(n_quest, n_latent, n_cat))
  for(j in 1:n_quest){
    for(k in 1:n_latent){
      for(r in 1:n_cat){
        U[j,k,r] <- psi[[j]][r, k]
      }
    }
  }

  list(
    converged=FALSE,
    convergence=sse_hist,
    lambda=weights,
    z=class,
    U=U,
    specific_params=list(
      k_hist=k_history[after_burnin],
      lambda_hist=weight_history[after_burnin],
      psi_hist=psi_history[after_burnin],
      pi_est_hist=pi_est_hist[after_burnin]
    )
  )

}
















