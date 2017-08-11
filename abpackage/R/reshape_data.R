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

#' Reshape data to have the structure required by \code{\link{PrePost}} and
#' \code{\link{PreCheck}}
#'
#' @param data A data.frame with several variables. The variable
#'   \code{condition} indicates the treatment assignment (\code{control} or
#'   \code{treatment}) for each row. The variable \code{observation} indicates
#'   the observation number of each row. The variable \code{pre.post} indicates
#'   the period (\code{pre} or \code{post}) for each row.
#'   The remaining variables represent the metrics to be tested. If there
#'   are multiple rows for a given combination of \code{condition},
#'   \code{observation} and \code{pre.post}, these rows are summed up.
#' @param condition.col The name of the variable indicating the treatment
#'   assignment for each row in \code{data}.
#' @param observation.col The name of the variable indicating the observation
#'   for each row in \code{data}.
#' @param pre.post.col The name of the variable indicating the period for each
#'   row in \code{data}.
#' @param condition.levels The names indicating \code{control} and
#'   \code{treatment} in \code{data}.
#' @param pre.post.levels The names indicating \code{pre} and \code{post} period
#'   in \code{data}.
#' @return
#'   A data.frame with several variables. The variable \code{condition}
#'   indicates the treatment assignment (\code{control} or \code{treatment})
#'   for each row. The variable \code{pre} indicates
#'   the observed value during the pre-period for each row.
#'   The variable \code{metric} indicates the name of each metric.
#' @export
#' @examples
#'   data <- SampleData(n.metrics = 4, spread = TRUE)
#'   reshaped.data <- ReshapeData(data)
#'   ans <- PrePost(reshaped.data)
ReshapeData <- function(data,
                        condition.col = "condition",
                        observation.col = "observation",
                        pre.post.col = "pre.post",
                        condition.levels = c("control", "treatment"),
                        pre.post.levels = c("pre", "post")) {

  # Standardize names
  data %<>%
    RenameColNames(condition.col,
                   "condition") %>%
    StandardizeLevelNames("condition",
                          condition.levels,
                          c("control", "treatment")) %>%
    RenameColNames(observation.col,
                   "observation") %>%
    RenameColNames(pre.post.col,
                   "pre.post") %>%
    StandardizeLevelNames("pre.post",
                          pre.post.levels,
                          c("pre", "post"),
                          sub.set = TRUE)

  # Ensure numeric for all metrics.
  assert_that(AreAllMetricsNumeric(data))

  # From vertical to horizontal structure: replace several metric columns
  # with colums metric and value.
  data %<>%
    tidyr::gather(metric, value, -pre.post, -condition, -observation)

  # Aggregate in case of multiple rows with the same metric, condition,
  # and observation combination.
  # Replace column value with columns pre and post.
  # Drop observation column.
  data %<>%
    dplyr::group_by(metric, condition, observation, pre.post) %>%
    dplyr::summarise(value = sum(value)) %>%
    tidyr::spread(pre.post, value) %>%
    as.data.frame() %>%
    dplyr::select(-observation)

  return(data)
}

AreAllMetricsNumeric <- function(data) {
  kStringColsRegex <- "[^(pre.post|condition|observation)]"
  all(sapply(dplyr::select(data, dplyr::matches(kStringColsRegex)), is.numeric))
}

on_failure(AreAllMetricsNumeric) <- function(call, env) {
  "All metrics should be numeric"
}
