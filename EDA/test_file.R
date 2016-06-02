library("testthat")

cat("\nRunning unit tests\n")
source("process_campaign.R")
source("mean_median_auc.R")
source("highest_correlation.R")

test_that("Process Campaign", {
    cat("\nTesting process_campaign.R \n")
    a <- process_campaign("exp-auc")
    expect_that(length(a), equals(7) )
    expect_that(median(a$num_trees), equals(40))
})
test_that("mean_median_auc", {
  cat("\nTesting mean_median_auc.R \n")
  a <- mean_median_auc("exp-auc")
  expect_that(nrow(a), equals(8) )
  expect_that(names(a), equals(c("num_trees","mean_auc","median_auc")))
})


test_that("highest_correlation", {
  cat("\nTesting highest_correlation.R \n")
  set.seed(1)
  vec <- 1:100
  vec2 <- 5 * rnorm(100)
  vec3 <- 2.0 + 5 * vec + 3 * rnorm(100)
  d <- data.frame(vec, vec2, vec3)
  l <- highest_correlation(d, "vec")
  expect_that(as.character(l[1,"var_name"]), equals("vec3"))
    expect_equal(l[1,"cor"], 0.99, tolerance = .01)
  })

