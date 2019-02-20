# metrics

## Overview

This package implements a consistent interface to compute metrics for classification models. It also offers an easy way to visualize those metrics.

## Installation

To install the package, simply run the following from an R console:

```r
# install.packages("remotes")
remotes::install_github("thoera/metrics")
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
predictions <- data.frame(
  obs = abalone$Class,
  prob = predict(lr, abalone, type = "response")
)

# compute the metrics for a given threshold
compute_metrics(predictions, threshold = 0.3)

# compute the metrics for a sequence of thresholds
metrics <- compute_metrics(predictions, threshold = seq(0, 1, 0.01))

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
quantiles <- qcut(predictions)
lift <- compute_lift(quantiles)

# plot the "gain chart"
draw_lift(lift)
```
