# Copyright 2016-2018 Google Inc. All rights reserved.
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

#' Returns credible intervals from an object of class \code{ab}.
#'
#' @param object An object of class \code{ab}.
#' @param only.sig If TRUE, returns only confidence intervals associated to
#'   tests that are statistically significant after multiple testing correction.
#' @param percent.change If TRUE, returns credible intervals for the
#'   percent change. If FALSE, returns credible intervals for
#'   the difference.
#' @param metrics The names of the metrics to get. If missing, all metrics
#'   are returned.
#' @return A data.frame with the credible intervals,
#'   the point estimate and the metric name.
#' @export
#' @examples
#' data <- SampleData(n.metrics = 20)
#' ans <- PrePost(data)
#' GetCIs(ans)

GetCIs <- function(object, only.sig = FALSE, percent.change = TRUE, metrics) {

  assert_that(inherits(object, "ab"))
  assert_that(is.logical(only.sig))
  assert_that(is.logical(percent.change))

  which.type <- dplyr::if_else(percent.change, "percent.change", "difference")
  output <- object$cis %>%
    dplyr::filter(type == which.type,
                  !only.sig | significant)

  if (!missing(metrics)) {
    output %<>%
      dplyr::filter(metric %in% metrics)
  }

  return(output)
}
