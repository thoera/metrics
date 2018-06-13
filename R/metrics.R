#' Compute the class predicted for a given threshold
#'
#' Compute the class predicted for a given threshold.
#'
#' @param data A dataframe with the probabilities computed for the positive
#'   class.
#' @param threshold A numeric value (default = 0.5) to decide if an observation
#'   will be affected in the positive class.
#' @param pos_class_col A string (default = "Yes"). The column's name of the
#'   predicted probabilities.
#' @param pos_class A string (default = "Yes"). How the positive class is coded.
#' @param neg_class A string (default = "No"). How the negative class is coded.
#' @return A dataframe with class predicted.
#' @seealso \code{\link{compute_metrics}},
#'   \code{\link{compute_optimal_threshold}}, \code{\link{objective_threshold}}
#' @examples
#' # add an example here
#' @export
compute_predict_class <- function(
  data,
  threshold = 0.5,
  pos_class_col = "Yes",
  pos_class = "Yes",
  neg_class = "No"
) {
  data[["pred"]] <- ifelse(data[[pos_class_col]] > threshold,
                           pos_class, neg_class)
  data[["pred"]] <- factor(data[["pred"]],
                           levels = c(pos_class, neg_class),
                           labels = c(pos_class, neg_class))
  return(data)
}

#' Compute several metrics for classification tasks.
#'
#' Compute the following metrics: "Accuracy", "Precision", "Recall",
#' "Specificity", "NPV" and "F1".
#' @param data A dataframe with the observed and the predicted class.
#' @param obs A string (default = "obs"). The column's name of the observed
#'   class.
#' @param pred A string (default = "pred"). The column's name of the predicted
#'   class.
#' @param pos_class A string (default = "Yes"). How the positive class is coded.
#' @param neg_class A string (default = "No"). How the negative class is coded.
#' @return A dataframe with the computed metrics.
#' @seealso \code{\link{compute_predict_class}},
#'   \code{\link{compute_optimal_threshold}}, \code{\link{objective_threshold}}
#' @examples
#' # add an example here
#' @export
compute_metrics <- function(
  data,
  obs = "obs",
  pred = "pred",
  pos_class = "Yes",
  neg_class = "No"
) {
  metrics <- vector("list", 6L)
  names(metrics) <- c("Accuracy", "Precision", "Recall",
                      "Specificity", "NPV", "F1")

  # accuracy: (TP + TN) / (TP + FP + TN + FN)
  metrics[["Accuracy"]] <- sum(data[[pred]] == data[[obs]]) / nrow(data)

  # precision: TP / (TP + FP)
  metrics[["Precision"]] <- sum(data[[pred]] == pos_class &
                                  data[[obs]] == pos_class) /
    sum(data[[pred]] == pos_class)

  # recall: TP / (TP + FN)
  metrics[["Recall"]] <- sum(data[[pred]] == pos_class &
                               data[[obs]] == pos_class) /
    sum(data[[obs]] == pos_class)

  # specifity: TN / (TN + FP)
  metrics[["Specificity"]] <- sum(data[[pred]] == neg_class &
                                    data[[obs]] == neg_class) /
    sum(data[[obs]] == neg_class)

  # negative predictive value (NPV): TN / (TN + FN)
  metrics[["NPV"]] <- sum(data[[pred]] == neg_class &
                            data[[obs]] == neg_class) /
    sum(data[[pred]] == neg_class)

  # F1 score: 2 * TP / (2 * TP + FP + FN)
  # or precision * recall / (precision + recall)
  metrics[["F1"]] <- 2 * metrics[["Precision"]] * metrics[["Recall"]] /
    (metrics[["Precision"]] + metrics[["Recall"]])

  return(metrics)
}

#' Compute the optimal threshold for the accuracy or the F1-score
#'
#' Compute the optimal threshold for the accuracy or the F1-score.
#'
#' @param data A dataframe with the metrics computed for different thresholds.
#' @param metric A string. The metric to maximize. One of "Accuracy" or "F1".
#' @return A dataframe with the optimal threshold(s).
#' @seealso \code{\link{compute_predict_class}}, \code{\link{compute_metrics}},
#'   \code{\link{objective_threshold}}
#' @examples
#' # add an example here
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
#' # add an example here
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
#' @return A ggplot2 object.
#' @seealso \code{\link{compute_predict_class}}, \code{\link{compute_metrics}}
#' @examples
#' # add an example here
#' @export
draw_metrics <- function(
  data,
  metric = c("Accuracy", "Precision", "Recall", "Specificity", "NPV", "F1")
) {
  if (!is.list(data)) {
    stop("'data' must be a list or a data frame")
  }

  # check if 'data' is a list of dataframes or not and reshape to long format
  if (inherits(data, "data.frame")) {
    data_lg <- data.table::melt(data, id = "threshold", variable = "metrics")
  } else {
    data_lg <- lapply(data, function(x) {
      data.table::melt(x, id = "threshold", variable = "metrics")
    })
    data_lg <- data.table::rbindlist(data_lg, idcol = "set")
  }

  # keep only the selected metrics
  data_lg <- data_lg[data_lg[["metrics"]] %in% metric, ]

  ggplot2::ggplot(data_lg,
                  ggplot2::aes_string(x = "threshold",
                                      y = "value",
                                      colour = "metrics")) +
    ggplot2::geom_line(size = 0.8, na.rm = TRUE) +
    ggplot2::labs(title = "Metrics", x = "threshold", y = "value") +
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
#' # add an example here
#' @export
draw_roc <- function(data, recall = "Recall", specificity = "Specificity") {
  if (!is.list(data)) {
    stop("'data' must be a list or a data frame")
  }

  # check if 'data' is a list of dataframes or not, select the columns,
  # reshape the data and plot it
  if (inherits(data, "data.frame")) {
    data <- data[, c(recall, specificity)]

    g <- ggplot2::ggplot(data,
                         ggplot2::aes_(x = quote(1 - Specificity),
                                       y = quote(Recall))) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1),
                            linetype = "longdash", color = "grey70") +
      ggplot2::geom_line(color = "#f8766d", size = 0.8)

  } else {
    data <- lapply(data, function(x) x[, c(recall, specificity)])
    data <- data.table::rbindlist(data, idcol = "set")

    g <- ggplot2::ggplot(data,
                         ggplot2::aes_(x = quote(1 - Specificity),
                                       y = quote(Recall))) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1),
                            linetype = "longdash", color = "grey70") +
      ggplot2::geom_line(ggplot2::aes_string(color = "set"), size = 0.8)
  }

  g + ggplot2::labs(title = "ROC curve",
                    x = "False Positive Rate (1 - Specificity)",
                    y = "True Positive Rate (Sensitivity)")
}

#' Compute the quantiles
#'
#' Compute the quantiles.
#'
#' @param data A dataframe with the probabilities for the positive class.
#' @param pos_class_col A string (default = "Yes"). The column's name of the
#'   predicted probabilities.
#' @param n A numeric value (default = 20). The number of quantiles to compute.
#' @return A dataframe with the quantiles computed.
#' @seealso \code{\link{compute_lift}}, \code{\link{draw_lift}}
#' @examples
#' # add an example here
#' @export
qcut <- function(data, pos_class_col = "Yes", n = 20L) {
  data[["quantile"]] <- (n + 1L) - findInterval(
    data[[pos_class_col]],
    stats::quantile(data[[pos_class_col]], seq(0L, 1L, length = n + 1L)),
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
#' # add an example here
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
#' @param type A string (default = "obs"). The column's name of the observed
#'   class.
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
#' # add an example here
#' @export
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
    # check if 'data' is a list and reshape the data
    if (inherits(data, "list")) {
      data <- data.table::rbindlist(data, idcol = "group")

      g <- ggplot2::ggplot(data, ggplot2::aes_(x = quote(quantile * 0.05),
                                               y = quote(cumulative_lift_per),
                                               color = quote(group))) +
        ggplot2::geom_line(size = 0.8, na.rm = TRUE)
    } else {
      g <- ggplot2::ggplot(data, ggplot2::aes_(x = quote(quantile * 0.05),
                                               y = quote(cumulative_lift_per))) +
        ggplot2::geom_line(color = "#f8766d", size = 0.8, na.rm = TRUE)
    }
    g <- g +
      ggplot2::geom_line(ggplot2::aes_(y = quote(cumulative_lift_per_ideal)),
                         size = 0.8, linetype = "longdash", color = "grey70") +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1),
                                  labels = scales::percent) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1),
                                  labels = scales::percent) +
      ggplot2::labs(title = "Cumulative gain chart",
                    x = "% of population", y = "% of target")
  }

  # lift curve
  if (type == "lift_curve") {
    # check if 'data' is a list and reshape the data
    if (inherits(data, "list")) {
      data <- data.table::rbindlist(data, idcol = "group")

      g <- ggplot2::ggplot(data, ggplot2::aes_(x = quote(quantile * 0.05),
                                               y = quote(lift),
                                               color = quote(group))) +
        ggplot2::geom_line(size = 0.8, na.rm = TRUE)
    } else {
      g <- ggplot2::ggplot(data, ggplot2::aes_(x = quote(quantile * 0.05),
                                               y = quote(lift))) +
        ggplot2::geom_line(color = "#f8766d", size = 0.8, na.rm = TRUE)
    }
    g <- g +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0.05, 1),
                                  labels = scales::percent) +
      ggplot2::labs(title = "Lift curve", x = "% of population", y = "Lift")
  }

  # cumulative lift curve
  if (type == "cumulative_lift_curve") {
    # check if 'data' is a list and reshape the data
    if (inherits(data, "list")) {
      data <- data.table::rbindlist(data, idcol = "group")

      g <- ggplot2::ggplot(data, ggplot2::aes_(x = quote(quantile * 0.05),
                                               y = quote(cumulative_lift),
                                               color = quote(group))) +
        ggplot2::geom_line(size = 0.8, na.rm = TRUE)
    } else {
      g <- ggplot2::ggplot(data, ggplot2::aes_(x = quote(quantile * 0.05),
                                               y = quote(cumulative_lift))) +
        ggplot2::geom_line(color = "#f8766d", size = 0.8, na.rm = TRUE)
    }
    g <- g +
      ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2), limits = c(0.05, 1),
                                  labels = scales::percent) +
      ggplot2::labs(title = "Cumulative lift curve",
                    x = "% of population", y = "Lift")
  }
  return(g)
}
