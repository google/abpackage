# Copyright 2014-2017 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

testthat::context("Unit tests for single_pre_post.R")

test_that("SinglePrePost", {
  n <- 100
  mu.pre <- 10
  mu.trmt <- 2
  mu.ctrl <- 0
  ctrl.data <- data.frame(pre = rnorm(n, mu.pre),
                          condition = rep("control", n),
                          stringsAsFactors = FALSE)
  ctrl.data$post <- ctrl.data$pre + rnorm(n, mu.ctrl)
  trmt.data <- data.frame(pre = rnorm(n, mu.pre),
                          condition = rep("treatment", n),
                          stringsAsFactors = FALSE)
  trmt.data$post <- trmt.data$pre + rnorm(n, mu.trmt)
  data <- dplyr::bind_rows(ctrl.data, trmt.data)
  ans <- SinglePrePost(data)
  expect_equal(dim(ans), c(2, 7))
  expect_equal(names(ans),
               c("lower", "median", "upper", "mean", "var", "p.value", "type"))
})

test_that("SinglePrePostWarnings", {
  data <- SampleData(n.metrics = 1) %>%
    dplyr::select(-pre) %>%
    dplyr::mutate(pre = 100,
                  metric = "users")
  expect_warning(SinglePrePost(data),
                 paste0("Standard deviation in the pre period is too small ",
                        "for the metric users. ",
                        "The pre period will not be used."))

  data <- SampleData(n.metrics = 1, n.observations = 3)
  expect_warning(SinglePrePost(data),
                 "Too few observations for the metric metric 1.")
  expect_equal(SinglePrePost(data), EmptyOutput())


  data <- SampleData(n.metrics = 1)
  data$post[1] <- NA
  expect_warning(SinglePrePost(data),
                 "Missing values for the metric metric 1 in the post period.")
  expect_equal(SinglePrePost(data), EmptyOutput())

  data <- SampleData(n.metrics = 1)
  data$pre[1] <- NA
  expect_warning(SinglePrePost(data),
                 paste0("Missing values for the metric metric 1 in the pre ",
                        "period. The pre period will not be used."))

  data <- SampleData(n.metrics = 1, mu.pre = 0, mu.trmt = 0, mu.ctrl = 0)
  expect_warning(SinglePrePost(data),
                 paste0("Non-positive values for the metric metric 1 in the ",
                        "post period."))
})

test_that("IsPrePeriodValid", {

  n <- 50
  condition.col <- rep(c("control", "treatment"), n)
  weights <- data.frame(control = 1, treatment = 1)
  data <- data.frame(pre = rep(2 * n, 10), condition = condition.col)
  expect_warning(IsPrePeriodValid(data, weights, "users"),
                 paste0("Standard deviation in the pre period is too small ",
                        "for the metric users. ",
                        "The pre period will not be used."))
  expect_false(IsPrePeriodValid(data, weights, "users"))

  data <- data.frame(pre = rnorm(2 * n, 100), condition = condition.col)
  data$pre[1] <- NA
  expect_warning(IsPrePeriodValid(data, weights, "metric 1"),
                 paste0("Missing values for the metric metric 1 in the pre ",
                        "period. The pre period will not be used."))
  expect_false(IsPrePeriodValid(data, weights, "metric 1"))

  data <- data.frame(pre = rnorm(2 * n, 100), condition = condition.col)
  expect_true(IsPrePeriodValid(data, weights, "metric 1"))
})

test_that("PostOnlyPosterior", {
  set.seed(2017)
  n <- 50
  n.nodes <- 50
  unif.nodes <- (2 * seq(1, n.nodes) - 1) / (2 * n.nodes)
  mu.ctrl <- 100
  mu.trmt <- 110
  sigma.ctrl <- 1
  sigma.trmt <- 2
  mus <- PostOnlyPosterior(rnorm(n, mu.trmt, sigma.trmt),
                           rnorm(n, mu.ctrl, sigma.ctrl),
                           unif.nodes)

  expect_equal(dim(mus), c(n.nodes ^ 2, 2))
  expect_equal(names(mus), c("ctrl", "trmt"))
  expect_equal(mean(mus$ctrl), mu.ctrl, scale = mu.ctrl, tolerance = 0.1)
  expect_equal(mean(mus$trmt), mu.trmt, scale = mu.trmt, tolerance = 0.1)
  expect_equal(sd(mus$ctrl),
               sigma.ctrl / sqrt(n),
               scale = sigma.ctrl / sqrt(n),
               tolerance = 0.1)
  expect_equal(sd(mus$trmt),
               sigma.trmt / sqrt(n),
               scale = sigma.trmt / sqrt(n),
               tolerance = 0.1)
})

test_that("PrePostPosterior", {
  n <- 50
  n.nodes <- 50
  unif.nodes <- (2 * seq(1, n.nodes) - 1) / (2 * n.nodes)
  mu.ctrl <- 100
  mu.trmt <- 110
  mu.pre <- 100
  sigma.ctrl <- 1
  sigma.trmt <- 2
  sigma.pre <- 1

  trmt.data <- data.frame(pre = rnorm(n, mu.pre, sigma.pre),
                          post = rnorm(n, mu.trmt, sigma.trmt))

  ctrl.data <- data.frame(pre = rnorm(n, mu.pre, sigma.pre),
                          post = rnorm(n, mu.ctrl, sigma.ctrl))

  mus <- PrePostPosterior(trmt.data,
                          ctrl.data,
                          data.frame(control = 1, treatment = 1),
                          unif.nodes)

  expect_equal(dim(mus), c(n.nodes ^ 3, 2))
  expect_equal(names(mus), c("ctrl", "trmt"))
  expect_equal(mean(mus$ctrl), mu.ctrl, scale = mu.ctrl, tolerance = 0.1)
  expect_equal(mean(mus$trmt), mu.trmt, scale = mu.ctrl, tolerance = 0.1)
  # There is no correlation between pre and post, so there is no variance
  # reduction.
  expect_equal(sd(mus$ctrl),
               sigma.ctrl / sqrt(n),
               scale = sigma.ctrl / sqrt(n),
               tolerance = 0.1)
  expect_equal(sd(mus$trmt),
               sigma.trmt / sqrt(n),
               scale = sigma.trmt / sqrt(n),
               tolerance = 0.1)
})

test_that("IsIllConditionedMatrix", {
  x <- rnorm(100)
  expect_false(IsIllConditionedMatrix(x, 0.1))
  x[1] <- 1E8
  expect_true(IsIllConditionedMatrix(x, 0.1))
})

test_that("WeightedAverage", {
  x.1 <- c(100, 300)
  x.2 <- c(100, 200, 150)
  expect_equal(WeightedAverage(x.1, x.2, weight.1 = 1, weight.2 = 1),
               mean(c(x.1, x.2)))

  x.1 <- c(100)
  x.2 <- c(10)
  weight.1 <- 0.25
  weight.2 <- 0.75
  expect_equal(WeightedAverage(x.1, x.2, weight.1, weight.2),
               weight.1 * x.1 + weight.2 * x.2)
})

test_that("WeightedSE", {
  x.1 <- c(100, 300, 200)
  x.2 <- c(100, 200, 150)
  x <- c(x.1, x.2)
  expect_equal(WeightedSE(x.1, x.2, weight.1 = 10, weight.2 = 10),
               sd(x) / sqrt(length(x)))
})
