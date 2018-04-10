

#' Predict the class of a new observation.
#'
#' @param object the result from a call to \code{mixdir()}. It needs to have the
#'   fields lambda and category_prob. lambda is a vector of probabilities for each category.
#'   category_prob a list of a list of a named vector with probabilities
#'   for each feature, latent class and possible category.
#' @param newdata a named vector with a single new observation or a data.frame
#'   with the same structure as the original data used for fitting the model.
#'   Missing features or features not encountered during training are replaced by
#'   NA.
#' @param ... currently unused
#'
#'
#' @return A matrix of with the same number of rows as the input and one column for each latent class.
#'
#' @examples
#'   data("mushroom")
#'   X <- as.matrix(mushroom)[1:30, ]
#'
#'   res <- mixdir(X)
#'
#'   # Predict Class
#'   predict(res, mushroom[40:45, ])
#'   predict(res, c(`gill-color`="black"))
#' @export
predict.mixdir <- function(object, newdata, ...){
  if (missing(newdata) || is.null(newdata)) {
    object$class_prob
  }else{
    predict_class(newdata, object$lambda, object$category_prob)
  }

}


predict_class <- function(X, lambda, category_prob){

  # Cleaning parameters

  ## Convert X to standardized data.frame of characters
  categories <- lapply(category_prob, function(cat) names(cat[[1]]))
  if(is.vector(X)){
    tmp <- as.data.frame(as.list(X), stringsAsFactors = FALSE)
    colnames(tmp) <- names(X)
    X <- tmp
  }else if(is.list(X)){
    tmp <- as.data.frame(X, stringsAsFactors = FALSE)
    colnames(tmp) <- names(X)
    X <- tmp
  }else if(is.matrix(X)){
    tmp <- as.data.frame(X, stringsAsFactors = FALSE)
    colnames(tmp) <- colnames(X)
    X <- tmp
  }
  n_ind <- nrow(X)
  X <- lapply(names(categories), function(coln){
    vec <- X[[coln]]
    if(is.null(vec)){
      vec <- as.character(rep(NA, n_ind))
    }else{
      vec <- as.character(vec)
      pos_cats <- categories[[coln]]
      non_matches <- which(!vec %in% pos_cats & ! is.na(vec))
      if(length(non_matches) > 0){
        warning(paste0("The new data has a response (",
                       paste0("\"", unique(vec[non_matches]), "\"", collapse = ","),
                       ") for category \"", coln, "\" which is not in category_prob. ",
                       "Handling X[j, i] it as if it was missing.\n"))
        vec[non_matches] <- NA
      }
    }
    vec
  })
  X <- as.data.frame(X, stringsAsFactors=FALSE)
  colnames(X) <- names(categories)

  prob_z <- matrix(vapply(seq_along(lambda), function(k){
    ## p(\lambda|alpha) doesn't matter, because it is the same everywhere
    ## p(z | lambda)
    p_z_lambda <- lambda[k]
    ## p(U_{j,k}|beta) doesn't matter, because it is the same everywhere
    ## p(X_{i,j}|U_j,z)
    p_x_u <- exp(rowSums(log(matrix(vapply(colnames(X), function(j){
      if(! j %in% names(categories))
        stop(paste0("The new observation X contains a category ", j, " not in the data"))
      ifelse(is.na(X[ ,j]), 1, category_prob[[j]][[k]][X[, j]])
    }, FUN.VALUE=rep(0.0, times=n_ind)), nrow=n_ind))))

    p_z_lambda * p_x_u
  }, FUN.VALUE=rep(0.0, times=n_ind)), nrow=n_ind)

  prob_z / rowSums(prob_z)

}


#' Find the top predictive features and values for each latent class
#'
#' @param mixdir_obj the result from a call to \code{mixdir()}. It needs to have the
#'   fields lambda and category_prob. lambda a vector of probabilities for each category.
#'   category_prob a list of a list of a named vector with probabilities
#'   for each feature, latent class and possible category.
#' @param top_n the number of top answers per category that will be returned. Default: 10.
#'
#' @return A data frame with four columns: column, answer, class and probability.
#'   The probability column contains the chance that an observation belongs to
#'   the latent class if all that is known about that observation that
#'   \code{`column`=`category`}
#'
#' @seealso find_typical_features
#' @examples
#'   data("mushroom")
#'   res <- mixdir(mushroom[1:30, ], beta=1)
#'   find_predictive_features(res, top_n=3)
#'
#' @export
find_predictive_features <- function(mixdir_obj, top_n=10){

  lambda <- mixdir_obj$lambda
  category_prob <- mixdir_obj$category_prob

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




#' Find the most typical features and values for each latent class
#'
#' @param mixdir_obj the result from a call to \code{mixdir()}. It needs to have the
#'   fields lambda and category_prob. lambda a vector of probabilities for each category.
#'   category_prob a list of a list of a named vector with probabilities
#'   for each feature, latent class and possible category.
#' @param top_n the number of top answers per category that will be returned. Default: 10.
#'
#' @return A data frame with four columns: column, answer, class and probability.
#'   The probability column contains the chance to see the answer in that column.
#'
#' @seealso find_predictive_features
#' @examples
#'   data("mushroom")
#'   res <- mixdir(mushroom[1:30, ], beta=1)
#'   find_typical_features(res, top_n=3)
#'
#' @export
find_typical_features <- function(mixdir_obj, top_n=10){

  lambda <- mixdir_obj$lambda
  category_prob <- mixdir_obj$category_prob

  categories <- lapply(category_prob, function(x) names(x[[1]]))
  cat_length <- sapply(categories, length)
  all_columns <- unlist(sapply(seq_along(categories), function(i) rep(names(categories[i]), cat_length[i])))
  all_responses <- unlist(categories)
  stopifnot(length(all_columns) == length(all_responses))
  probabilites <- sapply(seq_along(all_responses), function(i){
    sapply(seq_along(lambda), function(k) category_prob[[all_columns[i]]][[k]][all_responses[i]])
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








































