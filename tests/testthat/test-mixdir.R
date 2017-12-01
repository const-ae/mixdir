
library(mixdir)
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






