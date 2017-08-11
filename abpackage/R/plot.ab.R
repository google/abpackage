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
#' @param title The title of the plot.
#' @param size Width of the credible intervals.
#' @param horizontal If TRUE, the credible intervals are horizontal.
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
                    title,
                    size = 1,
                    horizontal = TRUE,
                    ...) {

  ci.level <- object$ci.level

  if (percent.change) {
    object <- object$percent.change
  } else {
    object <- object$difference
  }

  if (only.sig) {
    object <- dplyr::filter(object, significant == TRUE)
  }

  if (!missing(metrics)) {
    object <- dplyr::filter(object, metric %in% metrics)
  }

  n <- nrow(object)
  if (n == 0) {
    stop("No metrics to plot.")
  }
  # Rename, for instance, from "2.5%" to "lower", "50%" to "center",
  # and "97.5%" to "upper".
  ci.names <- CINames(ci.level)
  object <- RenameColNames(object,
                           ci.names$num,
                           ci.names$string)

  # Keep only first kNChar characters of metric names. Add three dots at the
  # end of names with more than kNChar characters.
  # Classify CIs in positive, negative and neutral.
  # Scale CIs when plotting percent change.
  kNChar <- 25
  object %<>%
    mutate(metric = as.character(metric)) %>%
    mutate(metric = if_else(nchar(metric) <= kNChar,
                            metric,
                            paste0(substr(metric, 1, kNChar), "...")),
           significant = if_else(significant == TRUE,
                                 if_else(center > 0, "positive", "negative"),
                                 "neutral")) %>%
    rowwise() %>%
    mutate(upper = if_else(percent.change, upper / 100, upper),
           center = if_else(percent.change, center / 100, center),
           lower = if_else(percent.change, lower / 100, lower)) %>%
    ungroup()

  limits <- aes(ymax = upper, ymin = lower)
  myColors <- c("red3", "gray30", "green4")
  names(myColors) <- c("negative", "neutral", "positive")
  colScale <- scale_colour_manual(name = "significant",
                                  values = myColors)

  p <- ggplot(object, aes(y = center, x = metric, color = significant)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_point(size = size) + geom_errorbar(limits, size = size / 3) +
    colScale

  y.scale.name <- ifelse(percent.change, "Change (%)", "Difference")
  if (percent.change) {
    p <- p + scale_y_continuous(labels = scales::percent,
                                name = y.scale.name)
  } else {
    p <- p + scale_y_continuous(name = y.scale.name)
  }
  if (!missing(title)) {
    p <- p + ggtitle(title)
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
