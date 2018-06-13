# metrics

## Overview

This package implements a consistent interface to compute metrics for classification models. It also offers an easy way to visualize those metrics.

## Installation

To install the package, simply run the following from an R console:

```r
# install.packages("devtools")
devtools::install_github("thoera/metrics")
```

## Usage

```r
library("ggplot2")
library("metrics")

data("abalone")
str(abalone)

# recode the target
abalone$Class <- factor(abalone$Class,
                        levels = c("N", "P"), labels = c("No", "Yes"))

# compute the model
lr <- glm(Class ~ ., family = "binomial", data = abalone)

# compute the predictions
predict_proba <- data.frame(
  obs = abalone$Class,
  Yes = predict(lr, abalone, type = "response")
)

# compute the class predicted for a list of thresholds
predict_class <- lapply(seq(0, 1, 0.01), function(threshold) {
  compute_predict_class(data = predict_proba, threshold = threshold)
})

# compute the metrics for each threshold and bind the results in a dataframe
metrics <- data.table::setDF(
  data.table::rbindlist(
    lapply(predict_class, compute_metrics)
  )
)

# add the thresholds
metrics[["threshold"]] <- seq(0, 1, 1 / (nrow(metrics) - 1L))

# find the optimal threshold for the F1-Score
compute_optimal_threshold(metrics, metric = "F1")

# find the optimal threshold for a specific objective
objective_threshold(metrics, metric = "Precision", objective = 0.75)

# plot all the metrics
draw_metrics(metrics)

# or a subset
draw_metrics(metrics, metric = c("Precision", "Recall"))

# plot the roc curve
draw_roc(metrics)

# compute the lift
lift <- qcut(predict_proba)
lift <- compute_lift(lift)

# plot the "gain chart"
draw_lift(lift)
```
