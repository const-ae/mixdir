
library(mixdir)


context("Variational Inference")
test_that("VI works for simple models", {
  X <- create_data()
  set.seed(1)
  result <- mixdir(X, n_latent=3, select_latent=FALSE)
  expect_true(result$converged)
  assigned_cluster <- result$pred_class
  # Expect that ind 2 and 7 (CCC) are in the same cluster
  expect_equal(assigned_cluster[7], assigned_cluster[2])
  # Expect that ind 10 and 9 (ABB) are in the same cluster
  expect_equal(assigned_cluster[10], assigned_cluster[9])
  # Expect that ind 10 and 7 are in different clusters
  expect_true(assigned_cluster[10] != assigned_cluster[7])
})


test_that("VI can handle missign values", {
  X <- create_data()
  set.seed(1)
  X[sample(seq_along(X), 3, replace=FALSE)] <- NA
  result <- mixdir(X, n_latent=3, select_latent=FALSE)
  expect_true(result$converged)
  assigned_cluster <-result$pred_class
  # Expect that ind 2 and 7 (CCC) are in the same cluster
  expect_equal(assigned_cluster[7], assigned_cluster[2])
  # Expect that ind 10 and 9 (ABB) are in the same cluster
  expect_equal(assigned_cluster[10], assigned_cluster[9])
  # Expect that ind 10 and 7 are in different clusters
  expect_true(assigned_cluster[10] != assigned_cluster[7])
})

test_that("VI can handle more complex models", {
  tmp <- generate_categorical_dataset(n_ind=1000, n_quest=5, n_cat=3, n_true_classes=4)
  set.seed(1)
  expect_silent(result <- mixdir(tmp$X, n_latent=4, select_latent=FALSE))
})

test_that("VI can handle the mushroom dataset", {
  data("mushroom")
  set.seed(4)
  expect_silent(res <- mixdir(mushroom, n_latent=5))
})

context("Variational Inference DP")

test_that("VI DP works for simple models", {
  X <- create_data()
  set.seed(1)
  result <- mixdir(X, n_latent=10, select_latent = TRUE)
  expect_true(result$converged)
  assigned_cluster <-result$pred_class
  # Expect that ind 2 and 7 (CCC) are in the same cluster
  expect_equal(assigned_cluster[7], assigned_cluster[2])
  # Expect that ind 10 and 9 (ABB) are in the same cluster
  expect_equal(assigned_cluster[10], assigned_cluster[9])
  # Expect that ind 10 and 7 are in different clusters
  expect_true(assigned_cluster[10] != assigned_cluster[7])
})

test_that("VI DP can handle missing values", {
  X <- create_data()
  set.seed(1)
  X[sample(seq_along(X), 3, replace=FALSE)] <- NA
  result <- mixdir(X, n_latent=10, select_latent = TRUE)
  expect_true(result$converged)
  assigned_cluster <- result$pred_class
  # Expect that ind 2 and 7 (CCC) are in the same cluster
  expect_equal(assigned_cluster[7], assigned_cluster[2])
  # Expect that ind 10 and 9 (ABB) are in the same cluster
  expect_equal(assigned_cluster[10], assigned_cluster[9])
  # Expect that ind 10 and 7 are in different clusters
  expect_true(assigned_cluster[10] != assigned_cluster[7])
})


test_that("mixdir can handle missing values as category", {
  X <- create_data()
  set.seed(1)
  X[sample(seq_along(X), 3, replace=FALSE)] <- NA
  result <- mixdir(X, n_latent=10, select_latent = TRUE, na.handle = "category")
  expect_true(result$converged)
  assigned_cluster <- result$pred_class
  # Expect that ind 2 and 7 (CCC) are in the same cluster
  expect_equal(assigned_cluster[7], assigned_cluster[2])
  # Expect that ind 10 and 9 (ABB) are in the same cluster
  expect_equal(assigned_cluster[10], assigned_cluster[9])
  # Expect that ind 10 and 7 are in different clusters
  expect_true(assigned_cluster[10] != assigned_cluster[7])
})

test_that("mixdir repetitions selects the best run", {
  X <- create_data()
  set.seed(1)
  result <- mixdir(X, n_latent=10, select_latent = TRUE, repetions = 3)

  set.seed(1)
  result1 <- mixdir(X, n_latent=10, select_latent = TRUE)
  result2 <- mixdir(X, n_latent=10, select_latent = TRUE)
  result3 <- mixdir(X, n_latent=10, select_latent = TRUE)

  expect_equal(result$ELBO, max(c(result1$ELBO, result2$ELBO, result3$ELBO)))
})




context("Prediction")

test_that("prediction of class works", {

  data("mushroom")
  res <- mixdir(mushroom[1:30, ])

  # Predict Class
  expect_silent(predict_class(mushroom[40, ], res$lambda, res$category_prob))
  expect_silent(predict_class(c(`gill-color`="black"), res$lambda, res$category_prob))
  expect_warning(predict_class(mushroom[42, ], res$lambda, res$category_prob))

  tmp <- mushroom
  ind <- sapply(tmp, is.character)
  tmp[ind] <- lapply(tmp[ind], factor)
  res2 <- mixdir(tmp[1:30, ])
  expect_silent(predict_class(mushroom[42, ], res2$lambda, res2$category_prob))
})



test_that("finding the most representative answers works", {
  data("mushroom")
  res <- mixdir(mushroom[1:30, ], beta=1)
  find_representative_answers(res$lambda, res$category_prob, top_n=3)
})


