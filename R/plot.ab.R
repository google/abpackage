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

#' Plot credible intervals
#'
#' @param object An object of class \code{ab}.
#' @param only.sig If TRUE, plots only credible intervals associated to tests
#'   that are statistically significant.
#' @param percent.change If TRUE, plots the credible intervals for the
#'   percent change. If FALSE, plots the credible intervals for the difference.
#' @param metrics The names of the metrics to plot. If missing, all metrics
#'   are plotted.
#' @param legend If TRUE, plots a legend.
#' @param size Width of the credible intervals.
#' @param horizontal If TRUE, the credible intervals are horizontal.
#` @param n.char Truncates metric names longer than this value.
#'
#' @return
#'  Plot of confidence intervals.
#' @export
#' @examples
#'   data <- SampleData(n.metric = 3, mu.trmt = c(12, 10, 10))
#'   ans <- PrePost(data)
#'   plot(ans)
#'   plot(ans, horizontal = FALSE)
#'   plot(ans, only.sig = TRUE)

plot.ab <- function(object,
                    only.sig = FALSE,
                    percent.change = TRUE,
                    metrics,
                    legend = FALSE,
                    size = 1,
                    horizontal = TRUE,
                    n.char = 25,
                    ...) {
  # Not using piping here because it fails when metrics is missing.
  object <- GetCIs(object, only.sig, percent.change, metrics)

  if (nrow(object) == 0) {
    stop("No metrics to plot.")
  }

  # Keep only first n.char characters of metric names. Add three dots at the
  # end of names with more than n.char characters.
  # Classify CIs in positive, negative and neutral.
  # Scale CIs when plotting percent change.
  object %<>%
    dplyr::mutate(metric = as.character(metric)) %>%
    dplyr::mutate(metric = dplyr::if_else(nchar(metric) <= n.char,
                                   metric,
                                   paste0(substr(metric, 1, n.char), "...")),
                  significant = dplyr::if_else(significant == TRUE,
                                               dplyr::if_else(median > 0,
                                                              "positive",
                                                              "negative"),
                                               "neutral")) %>%
    dplyr::mutate(pc = percent.change,
                  upper = dplyr::if_else(pc, upper / 100, upper),
                  median = dplyr::if_else(pc, median / 100, median),
                  lower = dplyr::if_else(pc, lower / 100, lower)) %>%
    dplyr::select(-pc)

  limits <- aes(ymax = upper, ymin = lower)
  myColors <- c("red3", "gray30", "green4")
  names(myColors) <- c("negative", "neutral", "positive")
  colScale <- scale_colour_manual(name = "significant",
                                  values = myColors)

  p <- ggplot(object, aes(y = median, x = metric, color = significant)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point(size = size) + geom_errorbar(limits, size = size / 3) +
    colScale

  y.scale.name <- dplyr::if_else(percent.change, "Change (%)", "Difference")
  if (percent.change) {
    p <- p + scale_y_continuous(labels = scales::percent,
                                name = y.scale.name)
  } else {
    p <- p + scale_y_continuous(name = y.scale.name)
  }
  if (!legend) {
    p <- p + theme(legend.position = "none")
  }
  if (horizontal) {
    p <- p +
      scale_x_discrete(name = "", limits = rev(object$metric)) +
      coord_flip()
  } else {
    p <- p + scale_x_discrete(name = "")
  }
  return(p)
}
