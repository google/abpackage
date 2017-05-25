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

testthat::context("Unit tests for reshape_data.R")

test_that("ReshapeData", {
  set.seed(101)
  n.metrics <- 10
  n.observations <- 15
  data <- SampleData(n.metrics = n.metrics,
                     n.observations = n.observations,
                     spread = TRUE)
  expect_equal(nrow(data), n.observations * 4)
  expect_equal(ncol(data), n.metrics + 3)

  reshaped.data <- ReshapeData(data)
  ans <- PrePost(reshaped.data)
  expect_s3_class(ans, "ab")
  expect_equal(dim(ans$percent.change), c(n.metrics, 9))

  data <- SampleData(spread = TRUE)
  data[ ,5] <- as.character(data[, 5])
  expect_error(ReshapeData(data),
               "All metrics should be numeric")
})
