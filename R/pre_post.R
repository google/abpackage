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

#' Compute credible intervals
#'
#' @param data A data.frame with several variables. The variable
#'   \code{condition} indicates the treatment assignment (\code{control} or
#'   \code{treatment}) for each row. The variable \code{post} indicates
#'   the observed value during the experiment period for each row. The variable
#'   \code{pre} indicates the observed value during the pre-period for each row.
#'   If omitted, the pre-period is not used to estimate the treatment effect.
#'   The variable \code{metric} indicates the name of each metric.
#'   If omitted, all rows are assumed to be from the same metric.
#' @param weights An optional data.frame with two variables and a single row.
#'   The variables \code{control} and \code{treatment} indicate the weight of
#'   the two groups. The weight must be proportional to the traffic proportion
#'   of observations in the condition group.
#'   If NULL, equal weights are are used in the fitting process.
#' @param ci.level Confidence level for credible intervals.
#' @param p.threshold P-value threshold used to identify statistically
#' significant effects. See \code{\link[stats]{p.adjust}} function for more
#' details.
#' @param p.method Correction method for multiple comparison. See
#' \code{\link[stats]{p.adjust}} function for more details.
#' @param n.nodes Number of nodes used for estimation.
#' @return An object of class \code{ab}.
#' @details Compute percent change and difference estimation between treatment
#'   and control for one or several metrics.
#' @export
#' @examples
#' data <- SampleData(n.metrics = 20)
#' (ans <- PrePost(data))
#' plot(ans)
#'
#' data.no.pre <- dplyr::select(data, -pre)
#' (ans.no.pre <- PrePost(data.no.pre))
#' plot(ans.no.pre)
PrePost <- function(data,
                    weights = NULL,
                    ci.level = 0.95,
                    p.threshold = 0.05,
                    p.method = "none",
                    n.nodes = 50) {


  assert_that(n.nodes > 0)
  if (n.nodes < 50) {
    warning("Estimates can be unstable for n.nodes < 50.")
  }
  assert_that(p.threshold >= 0, p.threshold <= 1)
  assert_that(ci.level > 0, ci.level < 1)
  assert_that(has_name(data, "post"))
  assert_that(has_name(data, "condition"))
  assert_that(
    HasVariableWithAllLevels(data, "condition", c("control", "treatment")))
  if (is.null(weights)) {
    weights <- data.frame(control = 1, treatment = 1)
  }
  assert_that(has_name(weights, "control"))
  assert_that(has_name(weights, "treatment"))
  assert_that(nrow(weights) == 1, weights$control > 0, weights$treatment > 0)
  if (!has_name(data, "metric")) {
    data %<>% dplyr::mutate(metric = rep("metric", nrow(.)))
  }

  # Compute confidence intervals and p-values for each metric.
  results <- data %>% dplyr::group_by(metric) %>%
    do(SinglePrePost(.,
                     weights,
                     n.nodes,
                     ci.level)) %>%
    dplyr::ungroup() %>% as.data.frame()
  # Extract confidence intervals and p-values for percent change.
  # Identify significant metrics.
  percent.change <- ExtractCIs(results,
                               "percent.change",
                               ci.level,
                               p.threshold,
                               p.method)
  # Extract confidence intervals and p-values for difference.
  # Identify significant metrics.
  difference <- ExtractCIs(results,
                           "difference",
                           ci.level,
                           p.threshold,
                           p.method)
  # Organize output in a list.
  output <- list(percent.change = percent.change,
                 difference = difference,
                 p.threshold = p.threshold,
                 p.method = p.method,
                 ci.level = ci.level)
  class(output) <- "ab"

  return(output)
}

MultipleTesting <- function(p.values, p.threshold, p.method) {
  return(p.adjust(p.values, method = p.method) < p.threshold)
}

ExtractCIs <- function(data, ci.type, ci.level, p.threshold, p.method) {

  # Used to rename, for instance, from "lower" to "2.5%",
  # from "center" to "50%", ...
  ci.names <- CINames(ci.level)

  output <- data %>%
    dplyr::filter(type == ci.type) %>%
    dplyr::mutate(significant = MultipleTesting(p.value,
                                                p.threshold,
                                                p.method),
                  metric = as.character(metric)) %>%
    dplyr::select(metric, everything()) %>%
    RenameColNames(ci.names$string, ci.names$num) %>%
    dplyr::arrange(metric)

  return(output)
}
