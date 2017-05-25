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

testthat::context("Unit tests for format.ab.R")

test_that("format.ab", {

  set.seed(1)
  data <- SampleData(n.metrics = 2)
  ans <- PrePost(data, p.method = "hochberg")
  str <- c(paste0(paste0("Significant tests (p.threshold = 0.05, ",
                         "p.method = hochberg): 0 out of 2 (0.00%)."),
                  "\n\nCredible intervals for (%) percent change between ",
                  "treatment and control:\n"),
           "    metric   2.5%    50% 97.5% p.value significant\n",
           "1 metric 1 -0.454 -0.066 0.324   0.737            \n",
           "2 metric 2 -0.175  0.244 0.660   0.249            \n",
           "Significant metrics are identifed by *.")
  expect_output(cat(format.ab(ans)), cat(str))

  expect_output(cat(format.ab(ans, only.sig = TRUE)),
                cat(paste0("Significant tests (p.threshold = 0.05, ",
                           "p.method = hochberg): 0 out of 2 (0.00%).")))
})
