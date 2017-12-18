


#' Run mixture of Dirichlet model
#'
#' @param X A matrix of size (N_ind x N_quest) that contains the categorical responses.
#' @param n_latent The number of latent factors that are used to approximate the model.
#' @param alpha A vector of length latent. If it is NULL alpha is initialized to rep(1, times=latent).
#'   It serves as prior for the Dirichlet distributions over the latent groups. Large numbers
#'   favor an equal distributions of individuals to groups and small number favor putting most individuals
#'   in one latent group.
#' @param beta A vector of length latent. If it is NULL alpha is initialized to rep(0.1, times=N_cat), where
#'   N_cat is the number of different categories in X.
#'   It serves as a prior for the Dirichlet distributions over the categorical responses. Large numbers
#'   favor an equal distribution of responses for a question of the individuals in the same latent group,
#'   small numbers indicate that indiviudals of the same latent group usually answer a question the same way.
#' @param select_latent A boolean that indicates if the exact number n_latent should be used or if a Dirichlet
#'   Process prior is used that shrinkes the number of used latent variables appropriately (can be controlled
#'   with alpha=c(a1, a2) and beta). Default: FALSE.
#' @param max_iter The maximum number of iterations.
#' @param method The method that is used to fit the model, can either be variational inference ("vi"), variational
#'   inference with a dirichlet process prior ("vi_dp"), expectation maximization ("em") or Gibbs Sampling ("mcmc").
#' @param epsilon A number that indicates the numerical precision necessary to consider the algorithm converged.
#' @param ... Additional parameters passed on to the underlying functions. The parameters are verbose, phi_init,
#'   zeta_init.
#' @export
mixdir <- function(X,
                   n_latent=3,
                   alpha=NULL,
                   beta=NULL,
                   select_latent=FALSE,
                   max_iter=100,
                   method=c("vi", "vi_dp", "em", "mcmc"),
                   epsilon=1e-3,
                   ...){
  method <- match.arg(method)
  n_cat <- length(unique(c(X)))
  if(is.null(alpha)){
    alpha <- rep(1, n_latent)
  }
  if(is.null(beta)){
    beta <- rep(0.01, n_cat)
  }

  if(select_latent){
    if(method == "vi" || method == "vi_dp")
      method <- "vi_dp"
    else
      stop("select_latent=TRUE can only be used in combination with method='vi' or method='vi_dp' ")
  }

  if(method == "vi"){
    mixdir_vi(X, n_latent, alpha, beta, n_cat, max_iter, epsilon, ...)
  }else if(method == "vi_dp"){
    mixdir_vi_dp(X, n_latent, alpha, beta, n_cat, max_iter, epsilon, ...)
  }else if(method == "em"){
    if(any(alpha != 1)) warning("The EM algorithm cannot handle custom alpha parameters.")
    mixdir_em(X, n_latent, beta, n_cat, max_iter, epsilon, ...)
  }else if(method == "mcmc"){
    if(epsilon != 1e-3) warning("The MCMC algorithm is always run for the full number of iterations and thus ignore epsilon.")
    if(n_latent != 3) warning("The MCMC algorithm automatically infers the best number of latent variables, thus n_latent is ignored.")
    mixdir_mcmc(X, n_latent, alpha_a=alpha[1], alpha_b=alpha[2], beta, n_cat, max_iter, ...)
  }else{
    stop(paste("Cannot deal with choice of method ", method, ". The only valid options are 'vi', 'em' or 'mcmc'"))
  }

}




















