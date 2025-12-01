#' Plot Stability Scores as Bar Chart
#'
#' @description
#' Creates a bar plot showing the stability score (pi_j) for each variable.
#' Variables are sorted by stability score in descending order.
#'
#' @param stability_result Output from stability()
#' @param threshold Numeric: draw a horizontal line at this threshold. Default: 0.6
#' @param top_n Integer: only show top N variables. Default: NULL (show all)
#' @param col_stable Color for stable variables (>= threshold). Default: "steelblue"
#' @param col_unstable Color for unstable variables (< threshold). Default: "lightgray"
#' @param col_threshold Color for threshold line. Default: "red"
#'
#' @return NULL (creates a plot)
#' @export
plot_stability <- function(stability_result, threshold = 0.6, top_n = NULL,
                           col_stable = "steelblue", col_unstable = "lightgray",
                           col_threshold = "red") {

  if (!inherits(stability_result, "stability")) {
    stop("stability_result must be output from stability()")
  }

  pi <- stability_result$pi

  # Sort in descending order
  pi_sorted <- sort(pi, decreasing = TRUE)

  # Limit to top N if requested
  if (!is.null(top_n) && top_n < length(pi_sorted)) {
    pi_sorted <- pi_sorted[1:top_n]
  }

  # Create plot
  par(mar = c(8, 4, 4, 2))

  barplot(pi_sorted,
          main = "Variable Stability Scores",
          ylab = "Stability Score (pi)",
          las = 2,
          col = ifelse(pi_sorted >= threshold, col_stable, col_unstable),
          ylim = c(0, 1),
          cex.names = 0.8)

  # Add threshold line
  abline(h = threshold, lty = 2, col = col_threshold, lwd = 2)

  # Add legend
  legend("topright",
         legend = c(sprintf("Stable (pi >= %.2f)", threshold),
                    sprintf("Unstable (pi < %.2f)", threshold),
                    sprintf("Threshold (tau = %.2f)", threshold)),
         fill = c(col_stable, col_unstable, NA),
         border = c("black", "black", NA),
         lty = c(NA, NA, 2),
         col = c(NA, NA, col_threshold),
         lwd = c(NA, NA, 2),
         bty = "n")

  grid()
}


#' Plot AIC by Model Step
#'
#' @description
#' Creates a boxplot showing the distribution of AIC values at each step
#' of the multi-path search.
#'
#' @param path_result Output from build_paths()
#' @param col_box Color for boxplot fill. Default: "lightblue"
#' @param col_border Color for boxplot border. Default: "steelblue"
#' @param col_median Color for median line. Default: "red"
#' @param col_best Color for best AIC points. Default: "darkred"
#'
#' @return NULL (creates a plot)
#' @export
plot_aic_by_step <- function(path_result,
                             col_box = "lightblue", col_border = "steelblue",
                             col_median = "red", col_best = "darkred") {

  if (!inherits(path_result, "multipath")) {
    stop("path_result must be output from build_paths()")
  }

  # Extract AIC values by step
  aic_by_step <- list()

  for (k in seq_along(path_result$frontiers)) {
    step_models <- path_result$frontiers[[k]]
    aic_by_step[[k]] <- sapply(step_models, function(m) m$aic)
  }

  # Create boxplot
  boxplot(aic_by_step,
          main = "AIC Distribution by Model Size",
          xlab = "Model Size (Number of Predictors)",
          ylab = "AIC",
          col = col_box,
          border = col_border,
          las = 1)

  # Add line connecting medians
  medians <- sapply(aic_by_step, median)
  lines(1:length(medians), medians, col = col_median, lwd = 2)

  # Add points for minimum AIC at each step
  mins <- sapply(aic_by_step, min)
  points(1:length(mins), mins, pch = 19, col = col_best, cex = 1.2)

  # Legend
  legend("topright",
         legend = c("Median AIC", "Best AIC"),
         col = c(col_median, col_best),
         lty = c(1, NA),
         pch = c(NA, 19),
         lwd = c(2, NA),
         bty = "n")

  grid()
}


#' Extract All Models from Multi-Path Result
#'
#' @description
#' Extracts all unique models from a multi-path search result as a data frame.
#'
#' @param path_result Output from build_paths()
#'
#' @return A data frame with columns: model_id, size, variables, aic, step
#' @export
extract_all_models <- function(path_result) {

  if (!inherits(path_result, "multipath")) {
    stop("path_result must be output from build_paths()")
  }

  all_models <- list()

  for (k in seq_along(path_result$frontiers)) {
    step_models <- path_result$frontiers[[k]]

    for (model in step_models) {
      all_models[[model$model_id]] <- list(
        model_id = model$model_id,
        size = length(model$vars),
        variables = ifelse(length(model$vars) == 0,
                           "(intercept only)",
                           paste(model$vars, collapse = ", ")),
        aic = model$aic,
        step = k
      )
    }
  }

  # Convert to data frame
  result_df <- do.call(rbind, lapply(all_models, function(x) {
    data.frame(
      model_id = x$model_id,
      size = x$size,
      variables = x$variables,
      aic = x$aic,
      step = x$step,
      stringsAsFactors = FALSE
    )
  }))

  # Sort by AIC
  result_df <- result_df[order(result_df$aic), ]
  rownames(result_df) <- NULL

  return(result_df)
}


#' Compute Confusion Matrix and Metrics for Binary Classification
#'
#' @description
#' For logistic regression models, computes confusion matrix and various
#' performance metrics (accuracy, sensitivity, specificity, etc.)
#'
#' @param fitted_model A fitted glm object with family = binomial
#' @param X Matrix of predictors
#' @param y True binary response (0/1)
#' @param threshold Numeric: probability threshold for classification. Default: 0.5
#'
#' @return A list with components:
#'   \item{confusion_matrix}{2x2 confusion matrix}
#'   \item{metrics}{Data frame with accuracy, sensitivity, specificity, precision, F1, FDR}
#'   \item{predicted_probs}{Predicted probabilities}
#'   \item{predicted_class}{Predicted classes}
#'
#' @export
confusion_metrics <- function(fitted_model, X, y, threshold = 0.5) {

  # Check if model is logistic
  if (!inherits(fitted_model, "glm") ||
      fitted_model$family$family != "binomial") {
    stop("fitted_model must be a glm object with family = binomial")
  }

  # Get predicted probabilities
  if (length(coef(fitted_model)) == 1) {
    # Intercept only
    pred_probs <- predict(fitted_model, type = "response")
  } else {
    # Extract variable names from model
    var_names <- names(coef(fitted_model))[-1]  # Remove intercept
    var_names <- gsub("^X_sub", "", var_names)  # Clean names if needed

    # Handle different column name formats (dots vs underscores)
    actual_col_names <- colnames(X)
    matched_names <- character(length(var_names))

    for (i in seq_along(var_names)) {
      # Try exact match first
      if (var_names[i] %in% actual_col_names) {
        matched_names[i] <- var_names[i]
      } else {
        # Try matching by replacing dots with underscores or vice versa
        potential_match <- gsub("\\.", "_", var_names[i])
        potential_match2 <- gsub("_", ".", var_names[i])

        if (potential_match %in% actual_col_names) {
          matched_names[i] <- potential_match
        } else if (potential_match2 %in% actual_col_names) {
          matched_names[i] <- potential_match2
        } else {
          # If still no match, try finding similar names
          similar <- grep(gsub("[._]", "", var_names[i]),
                          gsub("[._]", "", actual_col_names),
                          ignore.case = TRUE)
          if (length(similar) > 0) {
            matched_names[i] <- actual_col_names[similar[1]]
          } else {
            matched_names[i] <- var_names[i]  # Use as is (will error if wrong)
          }
        }
      }
    }

    # Get data for prediction
    newdata <- as.data.frame(X[, matched_names, drop = FALSE])
    names(newdata) <- var_names  # Use model's expected names
    pred_probs <- predict(fitted_model, newdata = newdata, type = "response")
  }

  # Classify based on threshold
  pred_class <- ifelse(pred_probs >= threshold, 1, 0)

  # Ensure y is 0/1
  y_binary <- as.numeric(y)
  if (!all(y_binary %in% c(0, 1))) {
    stop("y must be binary (0/1)")
  }

  # Confusion matrix
  conf_mat <- table(Predicted = pred_class, Actual = y_binary)

  # Add row/column names if missing
  if (nrow(conf_mat) == 1) {
    if (rownames(conf_mat)[1] == "0") {
      conf_mat <- rbind(conf_mat, "1" = c(0, 0))
    } else {
      conf_mat <- rbind("0" = c(0, 0), conf_mat)
    }
  }
  if (ncol(conf_mat) == 1) {
    if (colnames(conf_mat)[1] == "0") {
      conf_mat <- cbind(conf_mat, "1" = c(0, 0))
    } else {
      conf_mat <- cbind("0" = c(0, 0), conf_mat)
    }
  }

  # Extract values
  TN <- conf_mat["0", "0"]
  FP <- conf_mat["1", "0"]
  FN <- conf_mat["0", "1"]
  TP <- conf_mat["1", "1"]

  # Compute metrics
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- TP / (TP + FN)  # Recall, TPR
  specificity <- TN / (TN + FP)  # TNR
  precision <- TP / (TP + FP)    # PPV
  F1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
  FDR <- FP / (TP + FP)          # False Discovery Rate

  # Handle NaN cases
  if (is.nan(sensitivity)) sensitivity <- 0
  if (is.nan(specificity)) specificity <- 0
  if (is.nan(precision)) precision <- 0
  if (is.nan(F1)) F1 <- 0
  if (is.nan(FDR)) FDR <- 0

  metrics_df <- data.frame(
    Metric = c("Accuracy", "Sensitivity", "Specificity",
               "Precision", "F1 Score", "FDR"),
    Value = c(accuracy, sensitivity, specificity,
              precision, F1, FDR)
  )

  # Return results
  result <- list(
    confusion_matrix = conf_mat,
    metrics = metrics_df,
    predicted_probs = pred_probs,
    predicted_class = pred_class
  )

  return(result)
}


#' Print Confusion Matrix Nicely
#'
#' @param conf_mat Confusion matrix from confusion_metrics()
#' @export
print_confusion_matrix <- function(conf_mat) {
  cat("\nConfusion Matrix:\n")
  cat("=================\n")
  cat("               Actual\n")
  cat("Predicted    0    1\n")
  cat("    0     ", sprintf("%4d %4d", conf_mat["0","0"], conf_mat["0","1"]), "\n")
  cat("    1     ", sprintf("%4d %4d", conf_mat["1","0"], conf_mat["1","1"]), "\n")
}
