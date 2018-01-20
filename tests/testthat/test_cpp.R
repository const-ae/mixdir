library(mixdir)



expec_log_xij <- function(x_ij, phi_jk, zeta_ik){
  if(is.na(x_ij)){
    1
  }else{
    zeta_ik * (digamma(phi_jk[x_ij]) - digamma(sum(phi_jk)))
  }
}


context("Rcpp Implementations")
test_that("expec_log_xij works", {
  data("mushroom")
  X <- mushroom[sample(1:nrow(mushroom), 100), 1:5]
  X2 <- as.matrix(purrr::map_df(purrr::map_df(X, as.factor), as.numeric))
  any(is.na(X2))
  set.seed(1)
  result <- mixdir(X, n_latent=3, select_latent=FALSE, max_iter = 2)

  phi <- result$specific_params$phi
  zeta <- result$class_prob
  omega <- result$specific_params$omega
  phia <- conv_phi_to_array(phi, ncol(X), 3)

  n_ind <- nrow(X)
  n_quest <- ncol(X)
  n_latent <- 3
  rv <- sum(sapply(1:n_ind, function(i) sum(sapply(1:n_quest, function(j) sum(sapply(1:n_latent, function(k)
        expec_log_xij(X2[i,j], phi[[j]][[k]], zeta[i,k]) ))))))

  cv <- expec_log_x_cpp(X2, as.numeric(phia), zeta, dim(phia)[1], dim(phia)[2], dim(phia)[3])
  expect_equal(rv, cv)
})


test_that("zeta update works", {
  data("mushroom")
  X <- mushroom[sample(1:nrow(mushroom), 1000), ]
  X <- as.matrix(purrr::map_df(purrr::map_df(X, as.factor), as.numeric))
  any(is.na(X))
  set.seed(1)
  result <- mixdir(X, n_latent=3, select_latent=FALSE, max_iter = 2)

  phi <- result$specific_params$phi
  zeta <- result$class_prob
  zeta2 <- result$class_prob
  omega <- result$specific_params$omega
  phia <- conv_phi_to_array(phi, ncol(X), 3)
  n_latent <- 3
  n_quest <- ncol(X)
  n_ind <- nrow(X)
  n_cat <- max(X, na.rm=TRUE)

  for(k in 1:n_latent){
    zeta[, k] <- sapply(1:n_ind, function(i){
      exp(digamma(omega[k]) - digamma(sum(omega)) + sum(sapply(1:n_quest, function(j){
        if(is.na(X[i,j])){
          # If X_ij is missing ignore it
          0
        }else{
          digamma(phi[[j]][[k]][X[i,j]]) - digamma(sum(phi[[j]][[k]]))
        }
      })) - 1)
    })
  }


  zeta2 <- update_zeta_cpp(zeta2, X, phia, omega, n_ind, n_quest, n_latent, n_cat)

  expect_equal(zeta, zeta2)
})




test_that("zeta update works for dp", {
  data("mushroom")

  X <- mushroom[sample(1:nrow(mushroom), 1000), ]
  X <- as.matrix(purrr::map_df(purrr::map_df(X, as.factor), as.numeric))

  n_latent <- 10
  n_quest <- ncol(X)
  n_ind <- nrow(X)
  n_cat <- max(X, na.rm=TRUE)


  set.seed(1)
  result <- suppressWarnings(mixdir(X, n_latent=n_latent, select_latent=TRUE, max_iter = 2))

  phi <- result$specific_params$phi
  zeta <- result$class_prob
  zeta2 <- result$class_prob
  kappa1 <- result$specific_params$kappa1
  kappa2 <- result$specific_params$kappa2
  phia <- conv_phi_to_array(phi, ncol(X), n_latent)

  for(k in 1:n_latent){
    zeta[, k] <- sapply(1:n_ind, function(i){
      exp(
        (digamma(kappa1[k]) - digamma(kappa1[k] + kappa2[k])) +
          (if(k > 1) sum(sapply(1:(k-1), function(kp) (digamma(kappa2[kp]) - digamma(kappa1[kp] + kappa2[kp])))) else 0) +
          sum(sapply(1:n_quest, function(j){
            if(is.na(X[i,j])){
              # If X_ij is missing ignore it
              0
            }else{
              digamma(phi[[j]][[k]][X[i,j]]) - digamma(sum(phi[[j]][[k]]))
            }
          })) - 1
      )
    })
  }

  zeta2 <- update_zeta_dp_cpp(zeta2, X, phia, kappa1, kappa2, n_ind, n_quest, n_latent, n_cat)

  expect_equal(rowSums(zeta), rowSums(zeta2))
  expect_equal(zeta, zeta2)
})


