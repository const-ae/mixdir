

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
    predict_class(newdata, object$lambda, object$category_prob, object$na_handle)
  }

}


predict_class <- function(X, lambda, category_prob, na_handle=c("ignore", "category")){

  # Cleaning parameters

  ## Convert X to standardized data.frame of characters
  categories <- lapply(category_prob, function(cat) names(cat[[1]]))
  if(is.null(na_handle)){
    na_handle <- "ignore"
  }else{
    na_handle <- match.arg(na_handle, c("ignore", "category"))
  }
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
      if(na_handle == "category"){
        vec[is.na(vec)] <- "(Missing)"
      }
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
#' @seealso \code{\link{find_typical_features}} \code{\link{find_defining_features}}
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
#' @seealso \code{\link{find_predictive_features}} \code{\link{find_defining_features}}
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




#' Find the n defining features
#'
#' Reduce the dimensionality of a dataset by calculating how important each feature is
#' for inferring the clustering.
#'
#'
#' @param mixdir_obj the result from a call to \code{mixdir()}. It needs to have the
#'   fields category_prob. category_prob a list of a list of a named vector with probabilities
#'   for each feature, latent class and possible category.
#' @param X the original dataset that was used for clustering.
#' @param n_features the number of dimensions that should be selected. If it is
#'   \code{Inf} (the default) all features are returned ordered by importance
#'   (most important first).
#' @param measure The measure used to assess the loss of clustering quality
#'   if a variable is removed. Two measures are implemented: "JS" short for
#'   Jensen-Shannon divergence comparing the original class probabilities
#'   and the new predicted class probabilities (smaller is better),
#'   "ARI" short for adjusted Rand index compares the overlap of the original
#'   and the predicted classes (requires the \code{mcclust} package) (1 is perfect,
#'   0 is as good as random).
#' @param subsample_size Running this method on the full dataset can be slow,
#'   but one can easily speed up the calculation by randomly selecting
#'   a subset of rows from X without usually disproportionately hurting the
#'   selection performance.
#' @param step_size The method can either remove each feature individually
#'   and return the n features that caused the greatest quality loss
#'   (\code{step=Inf}) or iteratively remove the least important one until
#'   the the size of the remaining features equal \code{n_features}
#'   (\code{step=1}). Using a smaller step size increases the sensitivity
#'   of the selection process, but takes longer to calculate.
#' @param exponential_decay Boolean or number. Alternative way of
#'   calculating how many features to remove each step. The default is
#'   to always remove the least important 50\% of the features
#'   (\code{exponential_decay=2}).
#' @param verbose Boolean indicating if status messages should be printed.
#'
#' @details Iteratively find the variable, whose removal least affects the
#'   clustering compared with the original. If \code{n_features} is a finite number
#'   the quality is a single number and reflects how good those n features maintain
#'   the original clustering. If \code{n_features=Inf}, the method returns all features
#'   ordered by decreasing importance. The accompanying quality vector contains the
#'   "cumulative" loss if the corresponding variable would be removed.
#'   Note that depending on the step size scheme the quality can differ. For example
#'   if all variables are removed in one step (\code{step_size=Inf} and
#'   \code{exponential_decay=FALSE}) the quality is not cumulative, but simply the
#'   quality of the clustering excluding the corresponding feature. In that
#'   sense the quality vector should not be used as a definitive answer, but
#'   should only be used as a guidance to see where there are jumps in the quality.
#'
#' @seealso \code{\link{find_predictive_features}} \code{\link{find_typical_features}}
#'
#' @examples
#'   data("mushroom")
#'   res <- mixdir(mushroom[1:100, ], n_latent=20)
#'   find_defining_features(res, mushroom[1:100, ], n_features=3)
#'   find_defining_features(res, mushroom[1:100, ], n_features=Inf)
#' @export
find_defining_features <- function(mixdir_obj, X,
                                  n_features=Inf, measure=c("JS", "ARI"),
                                  subsample_size=Inf,
                                  step_size=Inf, exponential_decay=TRUE,
                                  verbose=FALSE){

  # Initialize variables
  measure <- match.arg(measure, c("JS", "ARI"))
  if(measure == "ARI"){
    if(!requireNamespace("mcclust", quietly = TRUE)){
      stop("Measure ARI needs the mcclust package. Please install it and try again.")
    }
  }
  exponential_step <- if(exponential_decay == TRUE){
    2
  }else if(is.numeric(exponential_decay)){
    exponential_decay
  }else{
    NA
  }
  early_stop <- if(is.infinite(n_features)){
    0
  }else{
    n_features
  }
  subsample <- sample(seq_len(nrow(X)), size=min(nrow(X), subsample_size), replace=FALSE)

  p <- predict.mixdir(mixdir_obj, X[subsample, , drop=FALSE])

  remaining_vars <- colnames(X)
  removed_vars <- character(0)
  quality <- numeric(0)

  while(length(remaining_vars) > early_stop){
    diverg <- vapply(seq_along(remaining_vars), function(i){
      q <- predict.mixdir(mixdir_obj, X[subsample, setdiff(remaining_vars, remaining_vars[i]), drop=FALSE])
      if(measure == "JS"){
        # Jensen Shannon Divergence (small means variable doesn't influence clustering)
        sum((q * (log(q) - log(p)) + p * (log(p) - log(q)))) / 2
      }else if(measure == "ARI"){
        1 - mcclust::arandi(apply(q, 1, which.max), apply(p, 1, which.max))
      }else{
        stop(paste0("Unknwon measure: ", measure))
      }
    }, FUN.VALUE = 0.0)
    exp_step <- if(exponential_decay){
      floor(length(remaining_vars) * (1- 1/exponential_step))
    }else{
      Inf
    }
    last_rm_idx <- max(min(step_size, exp_step, length(remaining_vars) - early_stop), 1)
    removed_vars <- c(removed_vars, remaining_vars[order(diverg)[seq_len(last_rm_idx)]])
    quality <- c(quality, sort(diverg)[seq_len(last_rm_idx)])
    remaining_vars <- setdiff(colnames(X), removed_vars)
    if(verbose){
      message(paste0("Remaining variables: ", length(remaining_vars)))
    }
  }
  if(measure == "ARI"){
    quality <- 1 - quality
  }
  if(! is.infinite(n_features) && n_features != 0){
    q <- predict.mixdir(mixdir_obj, X[subsample, remaining_vars, drop=FALSE])
    quality <- if(measure == "JS"){
      sum((q * (log(q) - log(p)) + p * (log(p) - log(q)))) / 2
    }else if(measure == "ARI"){
      mcclust::arandi(apply(q, 1, which.max), apply(p, 1, which.max))
    }
    # Not perfect order but better than random
    list(features=remaining_vars[order(-diverg[seq_len(length(remaining_vars))])], quality=quality)
  }else{
    list(features=rev(c(removed_vars, remaining_vars)), quality=rev(quality))
  }
}

#' Plot cluster distribution for a subset of features features
#'
#' @param features a character vector with feature names
#' @param category_prob a list over all features containing a
#'   list of the probability of each answer for every class. It
#'   is usually obtained from the result of a call to \code{mixdir()}.
#' @param classes which latent classes are plotted. By default all.
#'
#' @examples
#'   data("mushroom")
#'   res <- mixdir(mushroom[1:100, ], n_latent=4)
#'   plot_features(c("bruises", "edible"), res$category_prob)
#'
#'   res2 <- mixdir(mushroom[1:100, ], n_latent=20)
#'   def_feats <- find_defining_features(res2, mushroom[1:100, ], n_features=Inf)
#'   plot_features(def_feats$features[1:6], category_prob = res2$category_prob,
#'                classes=which(res$lambda > 0.01))
#
#' @export
plot_features <- function(features, category_prob,
                                   classes=seq_len(length(category_prob[[1]]))){
  req_packages <- c("ggplot2", "dplyr", "tibble", "purrr", "tidyr", "utils")
  avail <- sapply(req_packages, requireNamespace)
  if(! all(avail)){
    stop("This method requires the package: ", paste0(req_packages[!avail], collapse = ",") ,". Please install it and try again")
  }
  class_names <- c(LETTERS, utils::combn(LETTERS, 2, paste0, collapse=""), utils::combn(LETTERS, 3, paste0, collapse=""))
  plot_data <- tidyr::unnest(tibble::tibble(
    feature=rep(factor(features, levels=features, ordered = TRUE), each=length(classes)),
    class=rep(class_names[classes], times=length(features)),
    value=purrr::map(purrr::flatten(lapply(category_prob[features], `[`, classes)), tibble::enframe, name="answer")
  ))
  # Necessary to make CRAN check happy
  value <- class <- answer <- NULL
  suppressWarnings(ggplot2::ggplot(plot_data, ggplot2::aes(x=class, y=answer, fill=class)) +
    ggplot2::geom_tile(ggplot2::aes(width=sqrt(value), height=sqrt(value))) +
    ggplot2::facet_wrap( ~ feature, scale="free_y"))
}































