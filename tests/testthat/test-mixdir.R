
library(mixdir)

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


context("Variational Inference")

test_that("VI works for simple models", {
  X <- create_data()
  set.seed(1)
  result <- mixdir(X, method="vi")
  expect_true(result$converged)
  assigned_cluster <- apply(result$z, 1, which.max)
  # Expect that ind 2 and 7 (CCC) are in the same cluster
  expect_equal(assigned_cluster[7], assigned_cluster[2])
  # Expect that ind 10 and 9 (ABB) are in the same cluster
  expect_equal(assigned_cluster[10], assigned_cluster[9])
  # Expect that ind 10 and 7 are in different clusters
  expect_true(assigned_cluster[10] != assigned_cluster[7])
})



context("Expectation Maximization")

test_that("EM works for simple models", {
  X <- create_data()
  set.seed(1)
  result <- mixdir(X, method="em")
  expect_true(result$converged)
  assigned_cluster <- apply(result$z, 1, which.max)
  # Expect that ind 2 and 7 (CCC) are in the same cluster
  expect_equal(assigned_cluster[7], assigned_cluster[2])
  # Expect that ind 10 and 9 (ABB) are in the same cluster
  expect_equal(assigned_cluster[10], assigned_cluster[9])
  # Expect that ind 10 and 7 are in different clusters
  expect_true(assigned_cluster[10] != assigned_cluster[7])
})




context("Gibbs Sampling")

test_that("MCMC works for simple models", {
  X <- create_data()
  set.seed(1)
  result <- mixdir(X, method="mcmc")
  expect_true(! result$converged)
  assigned_cluster <- result$z
  # Expect that ind 2 and 7 (CCC) are in the same cluster
  expect_equal(assigned_cluster[7], assigned_cluster[2])
  # Expect that ind 10 and 9 (ABB) are in the same cluster
  expect_equal(assigned_cluster[10], assigned_cluster[9])
  # Expect that ind 10 and 7 are in different clusters
  expect_true(assigned_cluster[10] != assigned_cluster[7])
})






