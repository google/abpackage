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

testthat::context("Unit tests for get_cis.R")

test_that("GetCIs", {

  set.seed(1234)
  n.metrics <- 25
  data <- SampleData(n.metrics = n.metrics)
  ans <- PrePost(data, p.method = "hochberg")
  expect_error(GetCIs(ans$cis))
  expect_error(GetCIs(ans, only.sig = "significant"))
  expect_error(GetCIs(ans, percent.change = "percent.change"))
  expect_equal(dim(GetCIs(ans)), c(n.metrics, 9))
  expect_equal(dim(GetCIs(ans, percent.change = FALSE)), c(n.metrics, 9))
  expect_equal(nrow(GetCIs(ans, only.sig = TRUE)), 0)
  ans.none <- PrePost(data, p.method = "none")
  sig.cis <- ans.none$cis %>%
    dplyr::filter(significant == TRUE,
                  type == "percent.change")

  expect_equal(nrow(GetCIs(ans.none, only.sig = TRUE)), nrow(sig.cis))
})
