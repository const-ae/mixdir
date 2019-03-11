


#' Cluster high dimensional categorical datasets
#'
#' @param X A matrix or data.frame of size (N_ind x N_quest) that contains the categorical responses.
#'   The values can be characters, integers or factors. The most flexibility is provided if factors are used.
#' @param n_latent The number of latent factors that are used to approximate the model. Default: 3.
#' @param alpha A single number or a vector of two numbers in case select_latent=TRUE. If it is NULL alpha
#'   is initialized to 1. It serves as prior for the Dirichlet distributions over the latent groups. They
#'   serve as pseudo counts of individuals per group.
#' @param beta A single number. If it is NULL beta is initialized to 0.1.
#'   It serves as a prior for the Dirichlet distributions over the categorical responses. Large numbers
#'   favor an equal distribution of responses for a question of the individuals in the same latent group,
#'   small numbers indicate that individuals of the same latent group usually answer a question the same way.
#' @param select_latent A boolean that indicates if the exact number n_latent should be used or if a Dirichlet
#'   Process prior is used that shrinks the number of used latent variables appropriately (can be controlled
#'   with alpha=c(a1, a2) and beta). Default: FALSE.
#' @param max_iter The maximum number of iterations.
#' @param epsilon A number that indicates the numerical precision necessary to consider the algorithm converged.
#' @param na_handle Either "ignore" or "category". If it is "category" all \code{NA}'s in the dataset are converted to
#'   the string "(Missing)" and treated as their own category. If it is "ignore" the \code{NA}'s are treated as missing completely
#'   at random and are ignored during the parameter updates.
#' @param repetitions A number specifying how often to repeat the calculation with different initializations. Automatically
#'   selects the best run (i.e. max(ELBO)). Default: 1.
#' @param ... Additional parameters passed on to the underlying functions. The parameters are verbose, phi_init,
#'   zeta_init and if select_latent=FALSE omega_init or if select_latent=TRUE kappa1_init and kappa2_init.
#'
#' @return A list that is tagged with the class "mixdir" containing 8 elements:
#'   \describe{
#'     \item{converged}{a boolean indicator if the model has converged}
#'     \item{convergence}{a numerical vector with the ELBO of each iteration}
#'     \item{ELBO}{the final ELBO of the converged model}
#'     \item{lambda}{a numerical vector with the \code{n_latent} class probabilities}
#'     \item{pred_class}{an integer vector with the the most likely class assignment
#'        for each individual.}
#'     \item{class_prob}{a matrix of size \code{n_ind x n_latent} which has for each
#'       individual the probability to belong to class k.}
#'     \item{category_prob}{a list with one entry for each feature (i.e. column of X).
#'       Each entry is again a list with one entry for each class, that contains the
#'       probability of individuals of that class to answer with a specific response.}
#'     \item{specific_params}{A list whose content depends on the parameter \code{select_latent}.
#'       If \code{select_latent=FALSE} it contains the two entries omega and phi which
#'       are the Dirichlet hyperparameters that the model has fitted. If \code{select_latent=TRUE}
#'       it contains kappa1, kappa2 and phi, which are the hyperparameters for the
#'       Dirichlet Process and the Dirichlet of the answer.}
#'     \item{na_handle}{a string indicating the method used to handle missing values. This
#'       is important for subsequent calls to \code{predict.mixdir}.}
#'   }
#'
#' @details The function uses a mixture of multinomials to fit the model.
#'   The full model specification is
#'   \deqn{ \lambda | \alpha \sim DirichletProcess(\alpha)}{lambda | alpha ~ DirichletProcess(alpha)}
#'   \deqn{ z_i | \lambda \sim Multinomial(\lambda)}{z_i | lambda ~ Multinomial(lambda)}
#'   \deqn{ U_{j,k} | \beta \sim Dirichlet(\beta)}{U_{j,k} | beta ~ Dirichlet(beta)}
#'   \deqn{ X_{i,j} | U_j, z_i=k \sim Multinomial(U_{j,k})}{X_{i,j} | U_j, z_i=k ~ Multinomial(U_{j,k})}
#'
#'   In case that \code{select_latent=FALSE} the first line is replaced with
#'
#'   \deqn{ \lambda | \alpha \sim Dirichlet(\alpha)}{lambda | alpha ~ Dirichlet(alpha)}
#'
#'   The initial inspiration came from  Dunson and Xing (2009) who proposed a Gibbs
#'   sampling algorithm to solve this model. To speed up inference
#'   a variational inference approach was derived and implemented in this package.
#'
#' @examples
#'   data("mushroom")
#'   res <- mixdir(mushroom[1:30, ])
#'
#' @references
#'  1. C. Ahlmann-Eltze and C. Yau, "MixDir: Scalable Bayesian Clustering for High-Dimensional Categorical Data", 2018 IEEE 5th International Conference on Data Science and Advanced Analytics (DSAA), Turin, Italy, 2018, pp. 526-539.
#'
#'  2. Dunson, D. B. and Xing, C. Nonparametric Bayes Modeling of Multivariate Categorical Data. J. Am. Stat. Assoc. 104, 1042–1051 (2009).
#'
#'  3. Blei, D. M., Ng, A. Y. and Jordan, M. I. Latent Dirichlet Allocation. J. Macine Learn. Res. 3, 993–1022 (2003).
#'
#'  4. Blei, D. M. and Jordan, M. I. Variational inference for Dirichlet process mixtures. Bayesian Anal. 1, 121–144 (2006).
#' @export
mixdir <- function(X,
                   n_latent=3,
                   alpha=NULL,
                   beta=NULL,
                   select_latent=FALSE,
                   max_iter=100,
                   epsilon=1e-3,
                   na_handle=c("ignore", "category"),
                   repetitions=1,
                   ...){


  na_handle <- match.arg(na_handle, c("ignore", "category"))




  # Create a numeric matrix with the entries 1 to N_cat_j
  X <- as.data.frame(X)

  X[colnames(X)] <- lapply(X[colnames(X)], function(col){
    if(! is.factor(col)){
      col <- as.factor(as.character(col))
    }
    if(na_handle == "category"){
      levels(col) <- c(levels(col), "(Missing)")
      col[is.na(col)] <- "(Missing)"
    }
    col
  })
  categories <- lapply(1:ncol(X), function(j)levels(X[, j]))

  # Check for empty category. That would make problems
  cat_lengths <- sapply(categories, length)
  if(any(cat_lengths == 0)) {
    stop("Column ", paste0(colnames(X)[cat_lengths == 0], collapse = ","), " is empty (i.e. all values are NA). Please fix this.")
  }

  X[colnames(X)] <- lapply(X[colnames(X)], function(col) as.numeric(col))
  X <- as.matrix(X)

  result <- NULL
  for(repit in seq_len(repetitions)){
    if(! select_latent){
      result_tmp <- mixdir_vi(X=X, n_latent=n_latent, alpha=alpha, beta=beta,
                categories=categories, max_iter=max_iter, epsilon=epsilon, ...)
    }else{
      result_tmp <- mixdir_vi_dp(X=X, n_latent=n_latent, alpha=alpha, beta=beta,
                   categories=categories, max_iter=max_iter, epsilon=epsilon, ...)
    }
    if(is.null(result)){
      result <- result_tmp
    }else if(result$ELBO < result_tmp$ELBO){
      result <- result_tmp
    }
  }
  result$na_handle <- na_handle
  class(result) <- "mixdir"
  result
}


#' @useDynLib mixdir
#' @importFrom Rcpp sourceCpp
NULL


















