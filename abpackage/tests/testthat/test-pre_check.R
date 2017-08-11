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

testthat::context("Unit tests for pre_check.R")

test_that("PreCheck", {
  set.seed(2016)
  n.metrics <- 100
  data <- SampleData(n.metrics = n.metrics)
  ans <- PreCheck(data, n.samples = 10000)

  p <- 0.1
  expect_equal(mean(ans$p.value < p), p, tolerance = 0.05)
})


test_that("FromValueToStars", {
  expect_equal(FromValueToStars(0.001), "***")
  expect_equal(FromValueToStars(0.04), "**")
  expect_equal(FromValueToStars(0.055), "*")
  expect_equal(FromValueToStars(0.2), "")
})

