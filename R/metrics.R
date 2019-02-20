#' Compute the class predicted for a given threshold
#'
#' Compute the class predicted for a given threshold.
#'
#' @param data A dataframe with the probabilities computed for the positive
#'   class.
#' @param threshold A numeric value (default = 0.5) to decide if an observation
#'   will be affected to the positive class.
#' @param prob A string (default = "prob"). The column's name of the
#'   predicted probabilities.
#' @param pos_class A string (default = "Yes"). How the positive class is coded.
#' @param neg_class A string (default = "No"). How the negative class is coded.
#' @return A dataframe with the class predicted.
#' @seealso \code{\link{compute_metrics}},
#'   \code{\link{compute_optimal_threshold}}, \code{\link{objective_threshold}}
#' @examples
#' data <- data.frame(
#'   obs = c(rep("Yes", 20), rep("No", 20)),
#'   prob = c(runif(n = 20, min = 0.3, max = 0.8),
#'            runif(n = 20, min = 0.1, max = 0.6))
#' )
#'
#' compute_predict_class(data)
#' @export
compute_predict_class <- function(
  data,
  threshold = 0.5,
  prob = "prob",
  pos_class = "Yes",
  neg_class = "No"
) {
  data[["pred"]] <- factor(data[[prob]] > threshold,
                           levels = c(TRUE, FALSE),
                           labels = c(pos_class, neg_class))
  return(data)
}

#' Compute several metrics for classification tasks.
#'
#' Compute the following metrics: "Accuracy", "Precision", "Recall",
#' "Specificity", "NPV" and "F1".
#' @param data A dataframe with the observed and the predicted class.
#' @param threshold A numerical value or a sequence of numerical values (default
#'   = 0.5). The threshold(s) to compute the metrics.
#' @param obs A string (default = "obs"). The column's name of the observed
#'   class.
#' @param prob A string (default = "prob"). The column's name of the predicted
#'   probabilities.
#' @param pos_class A string (default = "Yes"). How the positive class is coded.
#' @param neg_class A string (default = "No"). How the negative class is coded.
#' @return A dataframe with the computed metrics.
#' @seealso \code{\link{compute_predict_class}},
#'   \code{\link{compute_optimal_threshold}}, \code{\link{objective_threshold}}
#' @examples
#' data <- data.frame(
#'   obs = c(rep("Yes", 20), rep("No", 20)),
#'   prob = c(runif(n = 20, min = 0.3, max = 0.8),
#'            runif(n = 20, min = 0.1, max = 0.6))
#' )
#'
#' compute_metrics(data, threshold = 0.5)
#' compute_metrics(data, threshold = seq(0, 1, 0.1))
#' @export
compute_metrics <- function(
  data,
  threshold = 0.5,
  obs = "obs",
  prob = "prob",
  pos_class = "Yes",
  neg_class = "No"
) {
  metrics_ <- function(data = data, obs = obs,
                       pos_class = pos_class, neg_class = neg_class) {
    metrics <- vector("list", 6L)
    names(metrics) <- c("Accuracy", "Precision", "Recall",
                        "Specificity", "NPV", "F1")

    # True Positives (TP)
    TP <- sum(data[["pred"]] == pos_class & data[[obs]] == pos_class)

    # False Positives (FP)
    FP <- sum(data[["pred"]] == pos_class & data[[obs]] == neg_class)

    # True Negatives (TN)
    TN <- sum(data[["pred"]] == neg_class & data[[obs]] == neg_class)

    # False Negatives (FN)
    FN <- sum(data[["pred"]] == neg_class & data[[obs]] == pos_class)

    # accuracy: (TP + TN) / (TP + FP + TN + FN)
    metrics[["Accuracy"]] <- (TP + TN) / nrow(data)

    # precision: TP / (TP + FP)
    metrics[["Precision"]] <- TP / (TP + FP)

    # recall: TP / (TP + FN)
    metrics[["Recall"]] <- TP / (TP + FN)

    # specifity: TN / (TN + FP)
    metrics[["Specificity"]] <- TN / (TN + FP)

    # negative predictive value (NPV): TN / (TN + FN)
    metrics[["NPV"]] <- TN / (TN + FN)

    # F1 score: 2 * precision * recall / (precision + recall)
    metrics[["F1"]] <- 2 * metrics[["Precision"]] * metrics[["Recall"]] /
      (metrics[["Precision"]] + metrics[["Recall"]])

    return(metrics)
  }

  if (length(threshold) == 1) {
    data <- compute_predict_class(data = data,
                                  threshold = threshold, prob = prob,
                                  pos_class = pos_class, neg_class = neg_class)
    return(
      data.frame(
        metrics_(data = data, obs = obs,
                 pos_class = pos_class, neg_class = neg_class),
        threshold = threshold
      )
    )
  } else {
    data <- lapply(threshold, function(t) {
      compute_predict_class(data = data,
                            threshold = t, prob = prob,
                            pos_class = pos_class, neg_class = neg_class)
    })
    result <- data.table::setDF(
      data.table::rbindlist(
        lapply(data, metrics_, obs = obs,
               pos_class = pos_class, neg_class = neg_class)
      )
    )
    return(cbind(result, threshold = threshold))
  }
}

#' Compute the optimal threshold for the accuracy or the F1-score
#'
#' Compute the optimal threshold for the accuracy or the F1-score.
#'
#' @param data A dataframe with the metrics computed for different thresholds.
#' @param metric A string. The metric to maximize. One of: "Accuracy" or "F1".
#' @return A dataframe with the optimal threshold(s).
#' @seealso \code{\link{compute_predict_class}}, \code{\link{compute_metrics}},
#'   \code{\link{objective_threshold}}
#' @examples
#' data <- data.frame(
#'   obs = c(rep("Yes", 20), rep("No", 20)),
#'   prob = c(runif(n = 20, min = 0.3, max = 0.8),
#'            runif(n = 20, min = 0.1, max = 0.6))
#' )
#'
#' metrics <- compute_metrics(data, threshold = c(0.3, 0.7, 0.2))
#'
#' compute_optimal_threshold(metrics, metric = "F1")
#' @export
compute_optimal_threshold <- function(data, metric = c("Accuracy", "F1")) {
  metric <- match.arg(metric)

  data[which(data[[metric]] == max(data[[metric]], na.rm = TRUE)), ]
}

#' Find the threshold(s) for a given objective and a given metric.
#'
#' @param data A dataframe with the metrics computed for different thresholds.
#' @param metric A string. The metric to consider. One of: "Accuracy",
#'   "Precision", "Recall", "Specificity", "NPV" or "F1".
#' @param objective A numeric value. The objective to reach.
#' @return A dataframe with the optimal threshold(s).
#' @seealso \code{\link{compute_predict_class}}, \code{\link{compute_metrics}},
#'   \code{\link{compute_optimal_threshold}}
#' @examples
#' data <- data.frame(
#'   obs = c(rep("Yes", 20), rep("No", 20)),
#'   prob = c(runif(n = 20, min = 0.3, max = 0.8),
#'            runif(n = 20, min = 0.1, max = 0.6))
#' )
#'
#' metrics <- compute_metrics(data, threshold = c(0.3, 0.7, 0.2))
#'
#' objective_threshold(metrics, metric = "Precision", objective = 0.75)
#' @export
objective_threshold <- function(data, metric, objective) {
  data[which.min(abs(data[[metric]] - objective)), ]
}

#' Plot the selected metrics
#'
#' Plot the selected metrics.
#'
#' @param data A dataframe or a list of dataframes with the metrics computed.
#' @param metric A string or a vector of string. The metric(s) to plot.
#' @param threshold A string (default = "threshold"). The column's name of the
#'   threshold.
#' @return A ggplot2 object.
#' @seealso \code{\link{compute_predict_class}}, \code{\link{compute_metrics}}
#' @examples
#' data <- data.frame(
#'   obs = c(rep("Yes", 20), rep("No", 20)),
#'   prob = c(runif(n = 20, min = 0.3, max = 0.8),
#'            runif(n = 20, min = 0.1, max = 0.6))
#' )
#'
#' metrics <- compute_metrics(data, threshold = c(0.3, 0.7, 0.2))
#'
#' draw_metrics(metrics)
#' @export
#' @importFrom rlang .data
draw_metrics <- function(
  data,
  metric = c("Accuracy", "Precision", "Recall", "Specificity", "NPV", "F1"),
  threshold = "threshold"
) {
  if (!is.list(data)) {
    stop("'data' must be a list or a data frame")
  }

  # check if 'data' is a list of dataframes or not and reshape to long format
  if (inherits(data, "data.frame")) {
    data_lg <- data.table::melt(data, id = threshold, variable = "metrics")
  } else {
    data_lg <- lapply(data, function(x) {
      data.table::melt(x, id = "threshold", variable = "metrics")
    })
    data_lg <- data.table::rbindlist(data_lg, idcol = "set")
  }

  # keep only the selected metrics
  data_lg <- data_lg[data_lg[["metrics"]] %in% metric, ]

  ggplot2::ggplot(data_lg,
                  ggplot2::aes(x = .data[[threshold]],
                               y = .data[["value"]],
                               color = .data[["metrics"]])) +
    ggplot2::geom_line(size = 0.8, na.rm = TRUE) +
    ggplot2::labs(title = "Metrics", x = "Threshold", y = "Value", color = "") +
    # use facets if 'data' is a list
    {if (!inherits(data, "data.frame")) ggplot2::facet_wrap(~ set)}
}

#' Plot the ROC curve
#'
#' Plot the ROC curve.
#'
#' @param data A dataframe or a list of dataframes with the recall and the
#'   specificity.
#' @param recall A string (default = "Recall"). The column's name of the
#'   computed recall.
#' @param specificity A string (default = "Specificity"). The column's name of
#'   the computed specificity.
#' @return A ggplot2 object.
#' @seealso \code{\link{compute_predict_class}}, \code{\link{compute_metrics}}
#' @examples
#' data <- data.frame(
#'   obs = c(rep("Yes", 20), rep("No", 20)),
#'   prob = c(runif(n = 20, min = 0.3, max = 0.8),
#'            runif(n = 20, min = 0.1, max = 0.6))
#' )
#'
#' metrics <- compute_metrics(data, threshold = c(0.3, 0.7, 0.2))
#'
#' draw_roc(metrics)
#' @export
#' @importFrom rlang .data
draw_roc <- function(data, recall = "Recall", specificity = "Specificity") {
  if (!is.list(data)) {
    stop("'data' must be a list or a data frame")
  }

  # check if 'data' is a list of dataframes or not, select the columns,
  # reshape the data and plot it
  if (inherits(data, "data.frame")) {
    # data <- data[, c(recall, specificity)]

    g <- ggplot2::ggplot(data,
                         ggplot2::aes(x = 1 - .data[[specificity]],
                                      y = .data[[recall]])) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1),
                            linetype = "longdash", color = "grey70") +
      ggplot2::geom_line(color = "#f8766d", size = 0.8)

  } else {
    data <- lapply(data, function(x) x[, c(recall, specificity)])
    data <- data.table::rbindlist(data, idcol = "set")

    g <- ggplot2::ggplot(data,
                         ggplot2::aes(x = 1 - .data[[specificity]],
                                      y = .data[[recall]])) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1),
                            linetype = "longdash", color = "grey70") +
      ggplot2::geom_line(ggplot2::aes(color = .data[["set"]]), size = 0.8)
  }

  g + ggplot2::labs(title = "ROC curve",
                    x = "False Positive Rate (1 - Specificity)",
                    y = "True Positive Rate (Sensitivity)",
                    color = "")
}

#' Compute the quantiles
#'
#' Compute the quantiles.
#'
#' @param data A dataframe with the probabilities for the positive class.
#' @param prob A string (default = "prob"). The column's name of the
#'   predicted probabilities.
#' @param n A numeric value (default = 20). The number of quantiles to compute.
#' @return A dataframe with the quantiles computed.
#' @seealso \code{\link{compute_lift}}, \code{\link{draw_lift}}
#' @examples
#' data <- data.frame(
#'   obs = c(rep("Yes", 20), rep("No", 20)),
#'   prob = c(runif(n = 20, min = 0.3, max = 0.8),
#'            runif(n = 20, min = 0.1, max = 0.6))
#' )
#'
#' quantiles <- qcut(data, prob = "prob")
#' @export
qcut <- function(data, prob = "prob", n = 20L) {
  data[["quantile"]] <- (n + 1L) - findInterval(
    data[[prob]],
    stats::quantile(data[[prob]], seq(0L, 1L, length = n + 1L)),
    all.inside = TRUE
  )
  return(data)
}

#' Compute the lift
#'
#' Compute the lift and cumulative lift.
#'
#' @param data A dataframe with the observed class, the probabilities for the
#'   positive class and the quantiles.
#' @param obs A string (default = "obs"). The column's name of the observed
#'   class.
#' @param pos_class A string (default = "Yes"). How the positive class is coded.
#' @param quantile A string (default = "quantile"). The column's name of the
#'   quantiles.
#' @return A dataframe with the lift and the cumulative lift computed for each
#'   quantile.
#' @seealso \code{\link{compute_lift}}, \code{\link{draw_lift}}
#' @examples
#' data <- data.frame(
#'   obs = c(rep("Yes", 20), rep("No", 20)),
#'   prob = c(runif(n = 20, min = 0.3, max = 0.8),
#'            runif(n = 20, min = 0.1, max = 0.6))
#' )
#'
#' quantiles <- qcut(data, prob = "prob")
#' lift <- compute_lift(quantiles)
#' @export
compute_lift <- function(
  data,
  obs = "obs",
  pos_class = "Yes",
  quantile = "quantile"
) {
  # get the target rate
  target_rate <- sum(data[[obs]] == pos_class) / nrow(data)

  # compute the number of targets in each quantile
  data <- as.data.frame(
    table(quantile = data[[quantile]], obs = data[[obs]])
  )
  data <- data[data[[obs]] == pos_class, ]
  data[["quantile"]] <- seq_len(nrow(data))

  # compute the lift
  quantile_per <- (1L / nrow(data))
  data[["lift"]] <- data[["Freq"]] / sum(data[["Freq"]]) / quantile_per

  # compute the cumulative lift
  data[["cumulative_lift"]] <- cumsum(data[["lift"]]) /
    seq_along(data[["lift"]])

  data[["cumulative_lift_per"]] <- (data[["cumulative_lift"]] *
                                      data[["quantile"]] * quantile_per)

  # compute the lift for the ideal model
  max_lift <- 1L / target_rate * quantile_per

  data[["cumulative_lift_per_ideal"]] <- data[["quantile"]] * max_lift
  data[["cumulative_lift_per_ideal"]] <- ifelse(
    data[["cumulative_lift_per_ideal"]] > 1,
    1, data[["cumulative_lift_per_ideal"]]
  )

  data <- rbind(c(0, NA, NA, NA, NA, 0, 0), data)
  return(data)
}

#' Plot the lift curves
#'
#' Plot the lift curves.
#'
#' @param data A dataframe or a list of dataframes with the lift and the
#'   cumulative lift per quantile.
#' @param type A string (default = "gain_chart"). The type of plot. One of:
#'   "gain_chart", "lift_curve" or "cumulative_lift_curve".
#' @param quantile A string (default = "quantile"). The column's name of the
#'   quantiles.
#' @param lift A string (default = "lift"). The column's name of the lift
#' @param cumulative_lift A string (default = "cumulative_lift"). The column's
#'   name of the cumulative lift.
#' @param cumulative_lift_per A string (default = "cumulative_lift_per"). The
#'   column's name of the cumulative lift in percentage.
#' @param cumulative_lift_per_ideal A string (default =
#'   "cumulative_lift_per_ideal"). The column's name of the cumulative lift in
#'   percentage for the perfect model.
#' @return A ggplot2 object.
#' @seealso \code{\link{compute_lift}}, \code{\link{draw_lift}}
#' @examples
#' data <- data.frame(
#'   obs = c(rep("Yes", 20), rep("No", 20)),
#'   prob = c(runif(n = 20, min = 0.3, max = 0.8),
#'            runif(n = 20, min = 0.1, max = 0.6))
#' )
#'
#' quantiles <- qcut(data, prob = "prob")
#' lift <- compute_lift(quantiles)
#'
#' draw_lift(lift)
#' @export
#' @importFrom rlang .data
draw_lift <- function(
  data,
  type = c("gain_chart", "lift_curve", "cumulative_lift_curve"),
  quantile = "quantile",
  lift = "lift",
  cumulative_lift = "cumulative_lift",
  cumulative_lift_per = "cumulative_lift_per",
  cumulative_lift_per_ideal = "cumulative_lift_per_ideal"
) {
  type <- match.arg(type)

  if (!is.list(data)) {
    stop("'data' must be a list or a data frame")
  }

  # cumulative gain chart
  if (type == "gain_chart") {
    # check if 'data' is a list and reshape it
    if (inherits(data, "list")) {
      data <- data.table::rbindlist(data, idcol = "group")

      g <- ggplot2::ggplot(data,
                           ggplot2::aes(x = .data[[quantile]] * 0.05,
                                        y = .data[[cumulative_lift_per]],
                                        color = .data[["group"]])) +
        ggplot2::geom_line(ggplot2::aes(y = .data[[cumulative_lift_per_ideal]]),
                           size = 0.8, linetype = "longdash",
                           color = "grey70") +
        ggplot2::geom_line(size = 0.8, na.rm = TRUE)
    } else {
      g <- ggplot2::ggplot(data,
                           ggplot2::aes(x = .data[[quantile]] * 0.05,
                                        y = .data[[cumulative_lift_per]])) +
        ggplot2::geom_line(ggplot2::aes(y = .data[[cumulative_lift_per_ideal]]),
                           size = 0.8, linetype = "longdash",
                           color = "grey70") +
        ggplot2::geom_line(color = "#f8766d", size = 0.8, na.rm = TRUE)
    }
    g <- g +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1),
                                  labels = scales::percent) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1),
                                  labels = scales::percent) +
      ggplot2::labs(title = "Cumulative gain chart",
                    x = "% of population", y = "% of target", color = "")
  }

  # lift curve
  if (type == "lift_curve") {
    # check if 'data' is a list and reshape it
    if (inherits(data, "list")) {
      data <- data.table::rbindlist(data, idcol = "group")

      g <- ggplot2::ggplot(data,
                           ggplot2::aes(x = .data[[quantile]] * 0.05,
                                        y = .data[[lift]],
                                        color = .data[["group"]])) +
        ggplot2::geom_line(size = 0.8, na.rm = TRUE)
    } else {
      g <- ggplot2::ggplot(data,
                           ggplot2::aes(x = .data[[quantile]] * 0.05,
                                        y = .data[[lift]])) +
        ggplot2::geom_line(color = "#f8766d", size = 0.8, na.rm = TRUE)
    }
    g <- g +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0.05, 1),
                                  labels = scales::percent) +
      ggplot2::labs(title = "Lift curve",
                    x = "% of population", y = "Lift", color = "")
  }

  # cumulative lift curve
  if (type == "cumulative_lift_curve") {
    # check if 'data' is a list and reshape it
    if (inherits(data, "list")) {
      data <- data.table::rbindlist(data, idcol = "group")

      g <- ggplot2::ggplot(data,
                           ggplot2::aes(x = .data[[quantile]] * 0.05,
                                        y = .data[[cumulative_lift]],
                                        color = .data[["group"]])) +
        ggplot2::geom_line(size = 0.8, na.rm = TRUE)
    } else {
      g <- ggplot2::ggplot(data,
                           ggplot2::aes(x = .data[[quantile]] * 0.05,
                                        y = .data[[cumulative_lift]])) +
        ggplot2::geom_line(color = "#f8766d", size = 0.8, na.rm = TRUE)
    }
    g <- g +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0.05, 1),
                                  labels = scales::percent) +
      ggplot2::labs(title = "Cumulative lift curve",
                    x = "% of population", y = "Cumulative lift", color = "")
  }
  return(g)
}
