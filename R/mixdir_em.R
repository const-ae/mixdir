mixdir_em <- function(X, n_latent, alpha, beta, n_cat, max_iter, epsilon,
                      zeta_init=NULL, phi_init=NULL, verbose=FALSE){
  # Initialize the parameters
  n_ind <- nrow(X)
  n_quest <- ncol(X)
  if(is.null(zeta_init)){
    zeta_init <- extraDistr::rdirichlet(n_ind, rep(1, n_latent))
  }
  if(is.null(phi_init)){
    phi_init <- array(1, dim=c(n_quest, n_latent, n_cat))
  }
  lambda <- alpha/sum(alpha)
  z <- zeta_init
  phi <- phi_init

  # Run algorithm
  sse_hist <- rep(NA, max_iter)
  sse_hist[1] <- Inf
  converged <- FALSE
  iter <- 1
  while(iter <= max_iter & ! converged){


    # Update phi
    for(j in 1:n_quest){
      # Count the number of z assigned to each category
      counter <- matrix(0, nrow=n_latent, ncol=n_cat)
      for(i in 1:n_ind){
        # Sum over all all individuals for which latent class is h count number of A,B,C's
        counter[ ,X[i,j]] <- counter[ ,X[i,j]] + 1 * z[i,]  * lambda
      }

      phi[j,,] <- counter + do.call(rbind, lapply(1:n_latent, function(k)beta))

    }

    # Update z
    for(i in 1:n_ind){
      zi <- sapply(1:n_latent, function(k){
        prod(sapply(1:n_quest, function(j){
          x <- phi[j, k, ]
          answer <- rep(0, n_cat)
          answer[X[i,j]] <- 1
          extraDistr::ddirmnom(answer, 1, x)
        }))
      })
      z[i, ] <- (zi)/(sum(zi))
    }

    lambda <- colSums(z)/ sum(z)

    # Calculate likelihood
    sse <- sum(sapply(1:n_ind, function(i)
        sum(sapply(1:n_quest, function(j)
          log(sum(sapply(1:n_latent, function(k){
            answer <- rep(0, n_cat)
            answer[X[i,j]] <- 1
             z[i,k] * extraDistr::ddirmnom(answer, 1, phi[j,k, ])
          })))
        ))
      ))


    if(iter != 1 && ! is.infinite(sse) && abs(sse - sse_hist[iter - 1]) < epsilon) converged <- TRUE

    if(verbose && iter %% 10 == 0) message(paste0("Iter: ", iter, " SSE: ", formatC(sse, digits=4)))

    sse_hist[iter] <- sse
    iter <- iter + 1

  }

  sse_hist <- sse_hist[! is.na(sse_hist)]

  U <- array(NA, dim=c(n_quest, n_latent, n_cat))
  for(j in 1:n_quest){
    for(k in 1:n_latent){
      U[j, k, ] <- phi[j,k, ] / sum(phi[j,k,])
    }
  }

  list(
    converged=converged,
    convergence=sse_hist,
    lambda=lambda,
    z=z,
    U=U,
    specific_params=list(
      phi=phi
    )
  )

}



