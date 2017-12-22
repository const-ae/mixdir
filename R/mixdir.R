


#' Run mixture of Dirichlet model
#'
#' @param X A matrix or data.frame of size (N_ind x N_quest) that contains the categorical responses.
#' @param n_latent The number of latent factors that are used to approximate the model.
#' @param alpha A single number or a vector of two numbers in case select_latent=TRUE. If it is NULL alpha
#'   is initialized to 1. It serves as prior for the Dirichlet distributions over the latent groups. They
#'   serve as pseudo counts of individuals per group.
#' @param beta A single number. If it is NULL alpha is initialized to 0.1.
#'   It serves as a prior for the Dirichlet distributions over the categorical responses. Large numbers
#'   favor an equal distribution of responses for a question of the individuals in the same latent group,
#'   small numbers indicate that indiviudals of the same latent group usually answer a question the same way.
#' @param select_latent A boolean that indicates if the exact number n_latent should be used or if a Dirichlet
#'   Process prior is used that shrinkes the number of used latent variables appropriately (can be controlled
#'   with alpha=c(a1, a2) and beta). Default: FALSE.
#' @param max_iter The maximum number of iterations.
#' @param epsilon A number that indicates the numerical precision necessary to consider the algorithm converged.
#' @param na.handle Either "ignore" or "category". If it is "category" all NA's in the dataset are converted to
#'   the string "NA" and treated as their own category. If it is "ignore" the NA's are treated as missing completely
#'   at random and are ignored during the parameter updates.
#' @param ... Additional parameters passed on to the underlying functions. The parameters are verbose, phi_init,
#'   zeta_init and if select_latent=FALSE omega_init or if select_latent=TRUE kappa1_init and kappa2_init.
#' @export
mixdir <- function(X,
                   n_latent=3,
                   alpha=NULL,
                   beta=NULL,
                   select_latent=FALSE,
                   max_iter=100,
                   epsilon=1e-3,
                   na.handle=c("ignore", "category"),
                   ...){


  na.handle <- match.arg(na.handle, c("ignore", "category"))
  if(na.handle == "category"){
    X[is.na(X)] <- "NA"
  }

  categories <- lapply(1:ncol(X), function(j){
    if(is.factor(X[,j ])) as.character(levels(X[,j ]))
    else as.character(sort(unique(X[, j])))
  })

  X <- as.matrix(X)
  class(X) = "character"


  if(! select_latent){
    mixdir_vi(X=X, n_latent=n_latent, alpha=alpha, beta=beta,
              categories=categories, max_iter=max_iter, epsilon=epsilon, ...)
  }else{
    mixdir_vi_dp(X=X, n_latent=n_latent, alpha=alpha, beta=beta,
                 categories=categories, max_iter=max_iter, epsilon=epsilon, ...)
  }

}




















