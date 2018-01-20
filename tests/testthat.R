Sys.setenv("R_TESTS" = "")


library(testthat)
library(mixdir)

test_check("mixdir")
