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

testthat::context("Unit tests for helpers.R")

test_that("RenameColNames", {

  std.col <- "pets"
  current.col <- "animals"
  data <- data.frame(c(rep("cat", 3), rep("dog", 2)))
  names(data) <- current.col
  data.std <- RenameColNames(data, current.col, std.col)
  expect_true(std.col %in% names(data.std))

  data.t <- as.tibble(data)
  data.std <- RenameColNames(data.t, current.col, std.col)
  expect_true(std.col %in% names(data.std))
})

test_that("HasVariableWithAllLevels", {

  data <- data.frame(animals = c(rep("cat", 3), rep("dog", 2)))
  expect_true(HasVariableWithAllLevels(data, "animals", c("cat", "dog")))
  expect_false(HasVariableWithAllLevels(data, "animals", c("frog", "dog")))

  data.t <- tibble(animals = data$animals)
  expect_true(HasVariableWithAllLevels(data.t, "animals", c("cat", "dog")))

})

test_that("HasVariableWithSomeLevels", {
  data <- data.frame(animals = c(rep("cat", 3)))
  expect_true(HasVariableWithSomeLevels(data, "animals", c("cat", "dog")))

  expect_false(HasVariableWithSomeLevels(data, "animals", c("pig", "dog")))

  data.t <- tibble(animals = data$animals)
  expect_true(HasVariableWithSomeLevels(data.t, "animals", c("cat", "dog")))

})

test_that("StandardizeLevelNames", {

  data <- data.frame(animals = c(rep("cat", 3), rep("dog", 2)))
  data.std <- StandardizeLevelNames(data,
                                    "animals",
                                    c("cat", "dog"),
                                    c("c", "d"))
  expect_equal(data.std$animals, c(rep("c", 3), rep("d", 2)))

  data.std <- StandardizeLevelNames(as.tibble(data),
                                    "animals",
                                    c("cat", "dog"),
                                    c("c", "d"))
  expect_equal(data.std[["animals"]], c(rep("c", 3), rep("d", 2)))

  data <- data.frame(animals = c(rep("dog", 5)))
  data.std <- StandardizeLevelNames(data,
                                    "animals",
                                    c("cat", "dog"),
                                    c("c", "d"),
                                    sub.set = TRUE)
  expect_equal(data.std$animals, rep("d", 5))

  data.std <- StandardizeLevelNames(as.tibble(data),
                                    "animals",
                                    c("cat", "dog"),
                                    c("c", "d"),
                                    sub.set = TRUE)
  expect_equal(data.std[["animals"]], rep("d", 5))
})

test_that("CINames", {

  data <- data.frame(num = c("2.5%", "50%", "97.5%"),
                     string = c("lower", "median", "upper"),
                     stringsAsFactors = FALSE)
  expect_equal(CINames(0.95), data)

  expect_error(CINames(1.1),
               "ci.level not less than 1")
})
