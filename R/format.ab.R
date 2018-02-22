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

#' Format an \code{ab} object for pretty printing.
#'
#' @param object An object of class \code{ab}, usually, a result of a call to
#'   \code{\link{PrePost}}.
#' @param digits Number of significant digits for the output.
#' @param only.sig If TRUE, returns only credible intervals associated to tests
#'   that are statistically significant.
#' @param percent.change If TRUE, returns the credible intervals for the
#'   percent change. If FALSE, returns the credible intervals for
#'   the difference.
#' @export
format.ab <- function(object,
                      digits = 3,
                      only.sig = FALSE,
                      percent.change = TRUE,
                      ...) {
  assert_that(is.count(digits))
  assert_that(is.logical(only.sig))
  assert_that(is.logical(percent.change))

  # Used to rename, for instance, from "lower" to "2.5%",
  # from "median" to "50%", ...
  ci.names <- CINames(object$ci.level)
  # The only.sig filtering is done later in the code, after the fraction
  # of significant metrics is computed.
  data <- object %>%
    GetCIs(only.sig = FALSE, percent.change = percent.change) %>%
    dplyr::select(metric, lower, median, upper, p.value, significant) %>%
    dplyr::mutate_if(is.numeric, function(x) round(x, digits = digits)) %>%
    RenameColNames(ci.names$string, ci.names$num)
  # Create a table with number of significant / not significant metrics.
  table.tests <- table(data$significant)

  statement <-
    sprintf(paste0("Significant tests (p.threshold = %.2f, p.method = %s): ",
                   "%d out of %d (%.2f%%)."),
            object$p.threshold,
            object$p.method,
            table.tests["TRUE"],
            sum(table.tests),
            100 * table.tests["TRUE"] / sum(table.tests))

  if (only.sig) {
    data <- dplyr::filter(data, significant == TRUE) %>%
      dplyr::select(-significant)
  } else {
    data <- dplyr::mutate(data,
                          significant = ifelse(significant == TRUE, " * ", ""))
  }

  if (nrow(data) > 0) {
    temp.1 <- paste0(round(100 * object$ci.level),
                     "% credible intervals for (%) ")
    temp.2 <- ifelse(percent.change,
                     "percent change between treatment and control:\n",
                     "difference between treatment and control:\n")
    statement <- paste0(statement, "\n\n", temp.1, temp.2)

    data <- capture.output(data) %>% paste0("\n")
    final.sentence <- if_else(only.sig,
                              "Only significant metrics are shown.",
                              "Significant metrics are identified by *.")
    statement <- c(statement, data, final.sentence)
  }

  return(statement)
}
