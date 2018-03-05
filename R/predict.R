

#' Predict the class of a new observation.
#'
#' @param X a named vector of responses for which to predict the latent class.
#'   Values can be missing or it can just be the values for which data is
#'   available.
#' @param lambda a vector of probabilities for each category.
#' @param category_prob a list of a list of a named vector with probabilties
#'   for each answer, latent class and possible category. This
#'   is usually handed over from the result of a call to \code{mixdir()}
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


#' Find the top predictive features and values for each latent class
#'
#' @param lambda a vector of probabilities for each category.
#' @param category_prob a list of a list of a named vector with probabilties
#'   for each feature, latent class and possible category. This
#'   is usually handed over from the result of a call to \code{mixdir()}
#' @param top_n the number of top answers per category that will be returned. Default: 10.
#'
#' @return A data frame with four columns: column, category, class and probabilty.
#'   The probability column contains the chance that an observation belongs to
#'   the latent class if all that is known about that observation that
#'   \code{`column`=`category`}
#'
#' @examples
#'   data("mushroom")
#'   res <- mixdir(mushroom[1:30, ], beta=1)
#'   find_predictive_features(res$lambda, res$category_prob, top_n=3)
#'
#' @export
find_predictive_features <- function(x, ...){
  UseMethod("find_predictive_features", x)
}

#' @describeIn find_predictive_features find predictive features for generic combinations
#'   of a vector lambda and a list category_prob
find_predictive_features.default <- function(lambda, category_prob, top_n=10){

  categories <- lapply(category_prob, function(x) names(x[[1]]))
  cat_length <- sapply(categories, length)
  all_columns <- unlist(sapply(seq_along(categories), function(i) rep(names(categories[i]), cat_length[i])))
  all_responses <- unlist(categories)
  stopifnot(length(all_columns) == length(all_responses))
  probabilites <- sapply(seq_along(all_responses), function(i){
    predict_class(lambda=lambda, category_prob = category_prob, X=structure(all_responses[i], names=all_columns[i]))
  })
  rep_factor <- if(is.matrix(probabilites)) nrow(probabilites) else 1
  result <- data.frame(column=rep(all_columns, each=rep_factor),
             answer=rep(all_responses, each=rep_factor),
             class=1:rep_factor,
             probability=c(probabilites),
             stringsAsFactors = FALSE)

  result <- result[order(- result$probability), ]
  do.call(rbind, lapply(order(-lambda), function(k){
    tmp <- result[result$class == k, ]
    tmp[1:min(top_n, nrow(tmp)), ]
  }))
}

#' @describeIn find_predictive_features find_predictive_features find predictive features for
#'   an object of class mixdir
find_predictive_features.mixdir <- function(mixdir_obj, top_n=3){
  find_predictive_features(mixdir_obj$lambda, mixdir_obj$category_prob, top_n=top_n)
}


