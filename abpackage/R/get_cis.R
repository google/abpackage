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

#' Returns credible intervals from an object of class \code{ab}.
#'
#' @param object An object of class \code{ab}.
#' @param only.sig If TRUE, returns only confidence intervals associated to
#'   tests that are statistically significant after multiple testing correction.
#' @param percent.change If TRUE, returns credible intervals for the
#'   percent change. If FALSE, returns credible intervals for
#'   the difference.
#' @return A data.frame with four columns containing the credible intervals,
#'   the point estimate and the metric name.
#' @export
#' @examples
#' data <- SampleData(n.metrics = 20)
#' ans <- PrePost(data)
#' GetCIs(ans)

GetCIs <- function(object, only.sig = FALSE, percent.change = TRUE) {

  assert_that(inherits(object, "ab"))
  assert_that(is.logical(only.sig))
  assert_that(is.logical(percent.change))

  if (percent.change) {
    data <- object$percent.change
  } else {
    data <- object$difference
  }
  if (only.sig) {
    data <- dplyr::filter(data, significant == TRUE)
    if (nrow(data) == 0) {
      stop("None of the tests are significant.")
    }
  }
  return(dplyr::select(data, -p.value, -significant, -type, -mean, -var))
}
