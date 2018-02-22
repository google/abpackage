# Copyright 2014-2018 Google Inc. All rights reserved.
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

#' Print method for class \code{ab}
#'
#' @param  object An object of class \code{ab}, usually, a result of a call to
#'   \code{\link{PrePost}}.
#' @param digits Number of significant digits for the output.
#' @param only.sig If TRUE, returns only credible intervals associated to tests
#'   that are statistically significant.
#' @param percent.change If TRUE, returns the credible intervals for the
#'   percent change. If FALSE, returns the credible intervals for
#'   the difference.
#' @export
#' @examples
#'   data <- SampleData(n.metrics = 20)
#'   ans <- PrePost(data)
#'   ans
print.ab <- function(object,
                     digits = 3,
                     only.sig = FALSE,
                     percent.change = TRUE,
                     ...) {
  cat(format.ab(object, digits, only.sig, percent.change, ...), "\n")
}
