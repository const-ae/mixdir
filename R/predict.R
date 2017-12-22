

#' Predict the class of a new observation.
#'
#' @param X a named vector of responses for which to predict the latent class.
#'   Values can be missing or it can just be the values for which data is
#'   available.
#' @param lambda a vector of probabilities for each category.
#' @param category_prob a list of a list of a named vector with probabilties
#'   for each answer, latent class and possible category.
#'
#' @details Usually the \code{lambda} and \code{category_prob} from a call to
#'   \code{mixdir()} are used.
#'
#' @return A vector of \code{length(lambda)} with the probability that \code{X}
#'   is in that latent class.
#'
#' @examples
#'   data("mushroom")
#'   X <- as.matrix(mushroom)[1:30, ]
#'
#'   res <- mixdir(X)
#'
#'   # Predict Class
#'   predict_class(mushroom[40, ], res$lambda, res$category_prob)
#'   predict_class(c(`gill-color`="black"), res$lambda, res$category_prob)
#' @export
predict_class <- function(X, lambda, category_prob){

  # Cleaning parameters
  if((inherits(X, "matrix") || inherits(X, "data.frame")) && nrow(X) != 1){
    stop("Can only handle arguments X as matrix or data.frame if it has 1 row")
  }
  if(inherits(X, "matrix") && nrow(X) == 1){
    tmp <- c(X[1, ])
    names(tmp) <- colnames(X)
    X <- tmp
  }else if(inherits(X, "data.frame") && nrow(X) == 1){
    tmp <- unlist(X[1, ])
    names(tmp) <- colnames(X)
    X <- tmp
  }

  prob_z <- sapply(seq_along(lambda), function(k){


    ## p(\lambda|alpha) doesn't matter, because it is the same everywhere
    ## p(z | lambda)
    p_z_lambda <- lambda[k]
    ## p(U_{j,k}|beta) doesn't matter, because it is the same everywhere
    ## p(X_{i,j}|U_j,z)
    p_x_u <- prod(sapply(names(X), function(j){
      if(! j %in% names(category_prob))
        stop(paste0("The new observation X contains a category ", j, " not in the data"))
      if(! X[j] %in% names(category_prob[[j]][[k]])){
        if(k == 1) warning(paste0("The new observation has a response (", X[j], ") for category ", j, " which is not in category_prob. ",
                                  "Handling X[j] it as if it was missing."))
        1
      }else if(is.na(X[j])) 1
      else category_prob[[j]][[k]][X[j]]
    }))

    p_z_lambda * p_x_u
  })

  prob_z / sum(prob_z)

}


# predictive_answers <- function(lambda, U, categories, top_n=10){
#   enframe(categories, name="question", value="response") %>%
#     group_by(question) %>%
#     unnest() %>%
#     ungroup() %>%
#     group_by(question, response) %>%
#     mutate(class=list(1:n_latent),
#            probability=list(predict_class(lambda=lambda, U=U, X=structure(response, names=question)))) %>%
#     unnest() %>%
#     mutate(lambda=res$lambda[class]) %>%
#     arrange(-lambda, class, - probability) %>%
#     group_by(class) %>%
#     mutate(rank=rank(- probability, ties.method="first")) %>%
#     filter(rank <= top_n) %>%
#     select(- rank)
# }



