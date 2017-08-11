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

testthat::context("Unit tests for plot.ab.R")

test_that("plot.ab", {
  n.metrics <- 25
  data <- SampleData(n.metrics = n.metrics)
  ans <- PrePost(data, p.method = "hochberg")
  p <- plot(ans)
  expect_equal(p$labels, list(x = "metric",
                              y = "center",
                              colour = "significant",
                              yintercept = "yintercept",
                              ymax = "upper",
                              ymin = "lower"))

  expect_equal(dim(p$data), c(n.metrics, 9))
  expect_equal(p$data$type, rep("percent.change", n.metrics))

  p <- plot(ans, percent.change = FALSE)
  expect_equal(dim(p$data), c(n.metrics, 9))
  expect_equal(p$data$type, rep("difference", n.metrics))

  expect_error(plot(ans, only.sig = TRUE),
               "No metrics to plot.")
})
