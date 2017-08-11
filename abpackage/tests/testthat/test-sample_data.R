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

testthat::context("Unit tests for sample_data.R")

test_that("SampleData", {
  set.seed(101)
  n.metrics <- 10
  n.observations <- 15
  data <- SampleData(n.metrics = n.metrics,
                     n.observations = n.observations)
  expect_equal(nrow(data), n.metrics * n.observations * 2)
  expect_equal(ncol(data), 4)
})

test_that("SampleMetricData", {
  set.seed(102)
  n <- 10
  data <- data.frame(mu.pre = 100,
                     sigma.pre = 1,
                     rho.ctrl = 0.8,
                     rho.trmt = 0.1,
                     mu.ctrl = 100,
                     sigma.ctrl = 1,
                     mu.trmt = 100,
                     sigma.trmt = 1)
  output <- abpackage:::SampleMetricData(data, n)
  expect_equal(nrow(output), n * 2)
  expect_equal(ncol(output), 4)
})

test_that("SampleGroupData", {
  set.seed(103)
  n <- 100
  mu.pre <- 100
  sigma.pre <- 1
  rho <- 0.8
  mu.post <- 100
  sigma.post <- 2
  data <- abpackage:::SampleGroupData(mu.pre, sigma.pre, mu.post,
                                      sigma.post, rho, n)
  expect_equal(nrow(data), n)
  expect_equal(ncol(data), 2)
})

test_that("Vectorize", {

  x <- c(1, 2, 3)
  y <- abpackage:::Vectorize(x, length(x))
  expect_equal(x, y)

  z <- 3
  n <- 100
  v <- abpackage:::Vectorize(z, n)
  expect_equal(rep(z, n), v)
})

test_that("MetricNames", {
  n <- 9
  metrics <- abpackage:::MetricNames(n)
  expect_equal(metrics[1], "metric 1")
  expect_equal(metrics[n], "metric 9")

  n <- 10
  metrics <- abpackage:::MetricNames(n)
  expect_equal(metrics[1], "metric 01")
  expect_equal(metrics[n], "metric 10")
})
