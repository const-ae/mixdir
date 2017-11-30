


#' Run mixture of Dirichlet model
#'
#' @param X A matrix of size (N_ind x N_quest) that contains the categorical responses.
#' @param latent The number of latent factors that are used to approximate the model.
#' @param max_iter The maximum number of iterations.
#' @param method The method that is used to fit the model, can either be variational inference ("vi"),
#'   expectation maximization ("em") or Gibbs Sampling ("mcmc").
#'
#' @export
mixdir <- function(X, latent=3, max_iter=100, method=c("vi", "em", "mcmc")){
  method <- match.arg(method)
}













