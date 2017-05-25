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

#' Check pre-period alignment between treatment and control
#'
#' @param data A data.frame with several variables. The variable
#'   \code{condition} indicates the treatment assignment (\code{control} or
#'   \code{treatment}) for each row. The variable \code{pre} indicates
#'   the observed value during the pre-period for each row.
#'   The variable \code{metric} indicates the name of each metric.
#'   If omitted, all rows are assumed to be from the same metric.
#' @param n.samples Number of samples used for the permutation.
#' @return A data.frame with a column metric, a column p-value and a column
#'   misalignemnt indicating the level of misalignemnt between the treatment
#'   and the control.
#' @details If all metrics are independent, 5% of metrics are expected
#'    to be classified as * (light misalignment), 4% of metrics are expected to
#'    be classified as ** (medium misalignment), and 1% of metrics are
#'    expected to be classified as *** (heavy misalignment). P-values are
#'    computed using \code{\link[coin]{oneway_test}}.
#' @export
#' @examples
#'   data <- SampleData(n.metrics = 20)
#'   (ans <- PreCheck(data))
PreCheck <- function(data,
                     n.samples = 50000) {

  assert_that(has_name(data, "pre"))
  assert_that(has_name(data, "condition"))
  assert_that(
    HasVariableWithAllLevels(data, "condition", c("control", "treatment")))

  if (!has_name(data, "metric")) {
    data %<>% dplyr::mutate(metric = rep("metric", nrow(.)))
  }

  output <- data %>%
    dplyr::mutate(condition = as.factor(condition)) %>%
    dplyr::group_by(metric) %>%
    do(PermutationTest(., n.samples = n.samples)) %>%
    dplyr::ungroup() %>% as.data.frame() %>%
    dplyr::arrange(metric) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(misalignment = FromValueToStars(p.value))

  row.names(output) <- NULL
  return(data.frame(output))
}

#' Compute permutation p-value for mean difference between treatment and control
#'
#' @param data A data.frame with 2 columns for each sample:
#'   A column named "condition" indicating whether the sample is from
#'   the control or treatment group.
#'   A column named "pre" indicating the observed value in the pre-period.
#' @param n.samples Number of samples used for the permutation.
#' @return A p-value associated to the null hypothesis that the two means are
#'   identical.
PermutationTest <- function(data,
                            n.samples = 50000) {
  ans <- oneway_test(pre ~ condition,
                     distribution = approximate(B = n.samples), data = data)
  p.value <- as.numeric(pvalue(ans))
  return(data.frame(p.value = p.value))
}

FromValueToStars <- function(x) {

  assert_that(is.numeric(x))

  kMisalignments <- data.frame(level = c(0.1, 0.05, 0.01),
                               star = c("*", "**", "***"),
                               stringsAsFactors = FALSE)
  output <- ""
  for (i in 1:nrow(kMisalignments)) {
    if (x <= kMisalignments$level[i]) {
      output <- as.character(kMisalignments$star[i])
    }
  }
  return(output)
}

