
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
