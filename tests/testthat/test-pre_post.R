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

testthat::context("Unit tests for pre_post.R")

test_that("SingleMetric", {
  data <- SampleData(n.metrics = 1)
  p.threshold <- 0.05
  ans <- PrePost(data, p.threshold = p.threshold)
  expect_s3_class(ans, "ab")
  expect_equal(ans$p.threshold, p.threshold)
  expect_true(all(ans$cis$p.value >= 0))
  expect_true(all(ans$cis$p.value <= 1))
  expect_true(all(ans$cis$significant %in% c(FALSE, TRUE)))
  expect_equal(dim(ans$cis), c(2, 9))
})

test_that("MultipleMetrics", {
  n.tests <- 100
  p.threshold <- 0.05
  data <- SampleData(n.metrics = n.tests)
  ans <- PrePost(data, p.threshold = p.threshold)
  expect_s3_class(ans, "ab")
  expect_true(all(ans$cis$p.value >= 0))
  expect_true(all(ans$cis$p.value <= 1))
  expect_true(all(ans$cis$significant %in% c(FALSE, TRUE)))
  expect_equal(dim(ans$cis), c(2 * n.tests, 9))
})

test_that("CoveragePostOnly", {
  set.seed(2)
  n.metrics <- 1000
  mu.pre <- 8
  mu.ctrl <- 10
  mu.trmt <- 8
  data <- SampleData(n.metrics = n.metrics,
                     mu.pre = mu.pre,
                     mu.ctrl = mu.ctrl,
                     mu.trmt = mu.trmt) %>% dplyr::select(-pre)
  ci.level <- 0.95
  ans <- PrePost(data, ci.level = ci.level)
  ci <- GetCIs(ans, percent.change = FALSE)
  diff <- mu.trmt - mu.ctrl
  coverage <- mean((ci$lower <= diff) & (ci$upper >= diff))
  expect_equal(coverage, ci.level, tolerance = 0.02)
})

test_that("CoveragePrePost", {
  set.seed(33)
  n.metrics <- 500
  mu.pre <- 8
  mu.ctrl <- 9
  mu.trmt <- 10
  sigma.trmt <- 1.1
  sigma.ctrl <- 1.0
  data <- SampleData(n.metrics = n.metrics,
                     mu.pre = mu.pre,
                     mu.ctrl = mu.ctrl,
                     mu.trmt = mu.trmt,
                     sigma.trmt = sigma.trmt,
                     sigma.ctrl = sigma.ctrl)
  ci.level <- 0.95
  ans <- PrePost(data, ci.level = ci.level)
  ci <- GetCIs(ans, percent.change = FALSE)
  diff <- mu.trmt - mu.ctrl
  coverage <- mean((ci$lower <= diff) & (ci$upper >= diff))
  expect_equal(coverage, ci.level, tolerance = 0.02)

  set.seed(33)
  n.metrics <- 500
  mu.pre <- 8
  mu.ctrl <- 9
  mu.trmt <- 10
  sigma.trmt <- 1.1
  sigma.ctrl <- 1.0
  data.1 <- SampleData(n.metrics = n.metrics,
                       mu.pre = mu.pre,
                       mu.ctrl = mu.ctrl,
                       mu.trmt = mu.trmt,
                       sigma.trmt = sigma.trmt,
                       sigma.ctrl = sigma.ctrl) %>%
    dplyr::mutate(observation = seq(1, nrow(.)))
  data.2 <- SampleData(n.metrics = n.metrics,
                       mu.pre = mu.pre,
                       mu.ctrl = mu.ctrl,
                       mu.trmt = mu.trmt,
                       sigma.trmt = sigma.trmt,
                       sigma.ctrl = sigma.ctrl) %>%
    dplyr::mutate(observation = seq(1, nrow(.))) %>%
    dplyr::filter(condition == "treatment")
  data <- dplyr::bind_rows(data.1, data.2) %>%
    dplyr::group_by(metric, condition, observation) %>%
    dplyr::summarise_each(funs(mean))
  ci.level <- 0.95
  w <- data.frame(control = 1, treatment = 2)
  ans <- PrePost(data, ci.level = ci.level, weights = w)
  ci <- GetCIs(ans, percent.change = FALSE)
  diff <- mu.trmt - mu.ctrl
  coverage <- mean((ci$lower <= diff) & (ci$upper >= diff))
  expect_equal(coverage, ci.level, tolerance = 0.02)
})

test_that("PrePostWarningsAndErrors", {

  n.metrics <- 1
  mu.pre <- 8
  mu.ctrl <- 9
  mu.trmt <- 10
  sigma.trmt <- 1.1
  sigma.ctrl <- 1.0
  data <- SampleData(n.metrics = n.metrics,
                     mu.pre = mu.pre,
                     mu.ctrl = mu.ctrl,
                     mu.trmt = mu.trmt,
                     sigma.trmt = sigma.trmt,
                     sigma.ctrl = sigma.ctrl)

  expect_warning(PrePost(data, n.nodes = 10),
                 "Estimates can be unstable for n.nodes < 50.")

  expect_error(PrePost(data, ci.level = 1.1),
               "ci.level not less than 1")

  expect_error(PrePost(data, p.threshold = -1),
               "p.threshold not greater than or equal to 0")

})



