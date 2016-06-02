library("testthat")
# test_dir(".")

cat("\nRunning unit tests\n")
source("mean_auc.R")
source("num_trees_auc.R")



test_that("mean_auc", {
    cat("\nTesting mean_auc.R \n")
    a <- mean_auc("exp-auc")
    expect_that(length(a), equals(2) )
    expect_that(names(a), equals(c("campaign","mean_auc")))
    expect_that(nrow(a), equals(47))
})


test_that("num_trees_auc", {
    cat("\nTesting num_trees_auc.R \n")
    a <- num_trees_auc("exp-auc", threshold=0.7)
    expect_that(nrow(a), equals(8))
    expect_that(names(a), equals(c("num_trees","mean_auc")))
    expect_that(median(a$num_trees), equals(25))
})


