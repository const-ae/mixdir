
create_data <- function(){
  data_df <- tibble::tribble(
    ~Individual, ~Question, ~Answer,
    "Ind1",      1,         "A",
    "Ind1",      2,         "B",
    "Ind1",      3,         "A",
    "Ind2",      1,         "C",
    "Ind2",      2,         "C",
    "Ind2",      3,         "C",
    "Ind3",      1,         "A",
    "Ind3",      2,         "B",
    "Ind3",      3,         "C",
    "Ind4",      1,         "C",
    "Ind4",      2,         "B",
    "Ind4",      3,         "A",
    "Ind5",      1,         "A",
    "Ind5",      2,         "B",
    "Ind5",      3,         "B",
    "Ind6",      1,         "A",
    "Ind6",      2,         "B",
    "Ind6",      3,         "B",
    "Ind7",      1,         "C",
    "Ind7",      2,         "C",
    "Ind7",      3,         "C",
    "Ind8",      1,         "A",
    "Ind8",      2,         "B",
    "Ind8",      3,         "B",
    "Ind9",      1,         "A",
    "Ind9",      2,         "B",
    "Ind9",      3,         "B",
    "Ind10",      1,         "A",
    "Ind10",      2,         "B",
    "Ind10",      3,         "B"
  )

  data_df$Answer=as.factor(data_df$Answer)

  N_ind <- length(unique(data_df$Individual))
  n_dim <- length(unique(data_df$Answer))

  Xn <- sapply(1:N_ind, function(i){
    as.numeric(sapply(1:n_dim, function(j)subset(data_df, Individual == paste0("Ind", i) & Question == j)$Answer))
  })

  t(Xn)

}



generate_categorical_dataset <- function(n_ind, n_quest, n_cat, n_true_classes, lambda_true=1/n_true_classes){

  if(length(n_cat) > 1 && length(n_cat) != n_quest) stop("n_cat must either be length or match the n_quest")
  if(length(n_cat) == 1){
    n_cat <- rep(n_cat, times=n_quest)
  }

  # First step generate true class representives
  U_true <- lapply(1:n_quest, function(j)lapply(1:n_true_classes, function(k) rep(NA, n_cat[j]) ))
  for(k in 1:n_true_classes){
    for(j in 1:n_quest){
      U_true[[j]][[k]] <- rmutil::rbetabinom(n_cat[j], 100, 0.05, 10) + 1
    }
  }

  if(length(lambda_true) == 1) {
    lambda_true <- rep(lambda_true, n_true_classes)
  }
  true_latent <- sample(1:n_true_classes, n_ind, replace=TRUE, prob=lambda_true)


  data <- matrix(NA, ncol=n_quest, nrow=n_ind)
  for(i in 1:n_ind){
    for(j in 1:n_quest){
      data[i, j] <- which(extraDistr::rdirmnom(1, 1, alpha=U_true[[j]][[true_latent[i]]]) == 1)
    }
  }

  list(X=data, true_latent=true_latent, U_true=U_true)

}
