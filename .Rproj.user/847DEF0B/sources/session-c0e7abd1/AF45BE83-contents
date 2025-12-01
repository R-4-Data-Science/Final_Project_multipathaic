#' Visualize Model Path Tree
#'
#' @description
#' Creates a tree diagram showing how models branch at each step.
#' This visualization helps understand the exploration of model space.
#'
#' @param path_result Output from build_paths()
#' @param max_models Integer: maximum models to show per step. Default: 15
#' @param highlight_best Logical: highlight the best path? Default: TRUE
#' @param col_models Color for regular models. Default: "lightgray"
#' @param col_best Color for best models. Default: "red"
#' @param col_line Color for best path line. Default: "red"
#'
#' @return A plot showing the model tree structure
#'
#' @export
plot_model_tree <- function(path_result, max_models = 15, highlight_best = TRUE,
                            col_models = "lightgray", col_best = "red", col_line = "red") {

  if (!inherits(path_result, "multipath")) {
    stop("path_result must be output from build_paths()")
  }

  # Extract data for plotting
  plot_data <- list()

  for (k in seq_along(path_result$frontiers)) {
    step_models <- path_result$frontiers[[k]]

    # Limit number of models shown
    if (length(step_models) > max_models) {
      aics <- sapply(step_models, function(m) m$aic)
      keep_idx <- order(aics)[1:max_models]
      step_models <- step_models[keep_idx]
    }

    for (i in seq_along(step_models)) {
      m <- step_models[[i]]
      plot_data[[length(plot_data) + 1]] <- data.frame(
        step = k,
        model_num = i,
        aic = m$aic,
        size = length(m$vars),
        vars = paste(m$vars, collapse = ","),
        stringsAsFactors = FALSE
      )
    }
  }

  df <- do.call(rbind, plot_data)

  # Find best path
  if (highlight_best && nrow(df) > 0) {
    best_idx <- which.min(df$aic[df$step == max(df$step)])
    best_aic <- df$aic[df$step == max(df$step)][best_idx]
    df$is_best <- df$aic == best_aic
  }

  # Create plot
  par(mar = c(5, 5, 4, 2))

  # Get unique steps
  steps <- unique(df$step)

  # Plot setup
  plot(0, 0, type = "n",
       xlim = c(0.5, max(steps) + 0.5),
       ylim = c(min(df$aic) - 5, max(df$aic) + 5),
       xlab = "Model Size (Number of Predictors)",
       ylab = "AIC",
       main = "Model Path Tree: Exploring Model Space",
       las = 1)

  grid()

  # Plot points
  for (s in steps) {
    step_data <- df[df$step == s, ]

    if (highlight_best) {
      # Non-best models
      points(rep(s, sum(!step_data$is_best)),
             step_data$aic[!step_data$is_best],
             pch = 21, bg = col_models, cex = 1.2)

      # Best models
      if (any(step_data$is_best)) {
        points(rep(s, sum(step_data$is_best)),
               step_data$aic[step_data$is_best],
               pch = 21, bg = col_best, cex = 1.5)
      }
    } else {
      points(rep(s, nrow(step_data)), step_data$aic,
             pch = 21, bg = col_models, cex = 1.2)
    }
  }

  # Add best AIC line
  if (nrow(df) > 0) {
    best_per_step <- tapply(df$aic, df$step, min)
    lines(as.numeric(names(best_per_step)), best_per_step,
          col = col_line, lwd = 2, lty = 2)
  }

  legend("topright",
         legend = c("Models explored", "Best at each step", "Best path"),
         pch = c(21, 21, NA),
         pt.bg = c(col_models, col_best, NA),
         lty = c(NA, NA, 2),
         col = c("black", "black", col_line),
         lwd = c(NA, NA, 2),
         bty = "n")
}


#' Visualize Variable Co-occurrence Heatmap
#'
#' @description
#' Creates a heatmap showing how often pairs of variables appear together
#' in the plausible models. This reveals variable relationships and redundancy.
#'
#' @param plausible_result Output from plausible_models()
#' @param col_palette Color palette for heatmap. Default: blue gradient.
#'   Can be a vector of colors or a single color name.
#'
#' @return A heatmap plot
#'
#' @export
plot_variable_heatmap <- function(plausible_result,
                                  col_palette = c("white", "lightblue", "blue", "darkblue")) {

  if (nrow(plausible_result) == 0) {
    stop("No plausible models to plot")
  }

  # Extract all variables across all models
  all_vars <- unique(unlist(strsplit(plausible_result$variables, ", ")))
  all_vars <- all_vars[all_vars != "(intercept only)"]

  if (length(all_vars) == 0) {
    message("Only intercept models found")
    return(invisible(NULL))
  }

  # Create co-occurrence matrix
  n_vars <- length(all_vars)
  co_occur <- matrix(0, n_vars, n_vars)
  rownames(co_occur) <- colnames(co_occur) <- all_vars

  # Fill matrix
  for (i in 1:nrow(plausible_result)) {
    vars_in_model <- unlist(strsplit(plausible_result$variables[i], ", "))
    vars_in_model <- vars_in_model[vars_in_model != "(intercept only)"]

    # Count co-occurrences
    if (length(vars_in_model) > 0) {
      for (v1 in vars_in_model) {
        for (v2 in vars_in_model) {
          co_occur[v1, v2] <- co_occur[v1, v2] + 1
        }
      }
    }
  }

  # Normalize by number of models
  co_occur <- co_occur / nrow(plausible_result)

  # Plot heatmap
  par(mar = c(8, 8, 4, 4))

  # Create color palette
  col_ramp <- colorRampPalette(col_palette)(100)

  image(1:n_vars, 1:n_vars, co_occur,
        col = col_ramp,
        xlab = "", ylab = "",
        main = "Variable Co-occurrence in Plausible Models",
        axes = FALSE)

  # Add axes
  axis(1, at = 1:n_vars, labels = all_vars, las = 2, cex.axis = 0.9)
  axis(2, at = 1:n_vars, labels = all_vars, las = 2, cex.axis = 0.9)

  # Add grid
  abline(h = 0:n_vars + 0.5, col = "gray90")
  abline(v = 0:n_vars + 0.5, col = "gray90")

  # Add values
  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (co_occur[i, j] > 0.1) {
        text(j, i, sprintf("%.2f", co_occur[i, j]),
             col = ifelse(co_occur[i, j] > 0.5, "white", "black"),
             cex = 0.7)
      }
    }
  }

  # Add color scale legend
  legend_vals <- seq(0, 1, length.out = 5)
  legend("bottom",
         legend = sprintf("%.2f", legend_vals),
         fill = col_ramp[seq(1, 100, length.out = 5)],
         horiz = TRUE,
         title = "Co-occurrence Proportion",
         xpd = TRUE, inset = c(0, -0.3),
         bty = "n")
}


#' Plot Model Comparison Dashboard
#'
#' @description
#' Creates a comprehensive dashboard showing model comparisons including
#' AIC, stability, and model composition.
#'
#' @param paths Output from build_paths()
#' @param stab Output from stability()
#' @param plausible Output from plausible_models()
#' @param col_stability Color for stability bars. Default: "steelblue"
#' @param col_size Color for size bars. Default: "coral"
#'
#' @export
plot_model_dashboard <- function(paths, stab, plausible,
                                 col_stability = "steelblue", col_size = "coral") {

  # Set up 2x2 plot layout
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

  # Plot 1: AIC by step
  plot_aic_by_step(paths)

  # Plot 2: Stability scores
  plot_stability(stab)

  # Plot 3: Plausible models comparison
  if (nrow(plausible) > 0) {
    barplot(plausible$avg_stability,
            names.arg = paste0("M", 1:nrow(plausible)),
            main = "Plausible Models: Average Stability",
            ylab = "Average Stability",
            col = col_stability,
            las = 2,
            ylim = c(0, 1))
    abline(h = 0.6, lty = 2, col = "red")
  }

  # Plot 4: Model sizes
  if (nrow(plausible) > 0) {
    barplot(plausible$size,
            names.arg = paste0("M", 1:nrow(plausible)),
            main = "Plausible Models: Size",
            ylab = "Number of Predictors",
            col = col_size,
            las = 2)
  }

  # Reset to single plot
  par(mfrow = c(1, 1))
}


#' Plot Bootstrap Stability Distribution
#'
#' @description
#' Shows the distribution of stability scores across bootstrap resamples
#' for each variable.
#'
#' @param stability_result Output from stability()
#' @param top_n Integer: show only top N variables. Default: 10
#' @param col_box Color for boxplots. Default: "lightblue"
#' @param col_border Color for box borders. Default: "steelblue"
#' @param col_mean Color for mean points. Default: "red"
#' @param col_threshold Color for threshold line. Default: "darkred"
#'
#' @export
plot_stability_distribution <- function(stability_result, top_n = 10,
                                        col_box = "lightblue", col_border = "steelblue",
                                        col_mean = "red", col_threshold = "darkred") {

  if (!inherits(stability_result, "stability")) {
    stop("stability_result must be output from stability()")
  }

  # Get top N variables by stability
  pi <- sort(stability_result$pi, decreasing = TRUE)
  if (length(pi) > top_n) {
    pi <- pi[1:top_n]
  }

  z_mat <- stability_result$z_matrix[, names(pi), drop = FALSE]

  # Create boxplot
  par(mar = c(8, 4, 4, 2))

  boxplot(z_mat,
          main = "Stability Distribution Across Bootstrap Resamples",
          ylab = "Proportion of Models Containing Variable",
          las = 2,
          col = col_box,
          border = col_border)

  # Add mean stability line for each variable
  means <- colMeans(z_mat)
  points(1:ncol(z_mat), means, pch = 18, col = col_mean, cex = 1.5)

  abline(h = 0.6, lty = 2, col = col_threshold, lwd = 2)

  legend("topright",
         legend = c("Bootstrap distribution", "Mean stability", "tau = 0.6"),
         pch = c(NA, 18, NA),
         lty = c(NA, NA, 2),
         col = c(col_border, col_mean, col_threshold),
         lwd = c(NA, NA, 2),
         bty = "n")
}


#' Create Variable Importance Ranking
#'
#' @description
#' Combines stability scores and AIC information to create a comprehensive
#' variable importance ranking.
#'
#' @param paths Output from build_paths()
#' @param stab Output from stability()
#'
#' @return Data frame with variable rankings
#'
#' @export
variable_importance_ranking <- function(paths, stab) {

  # Count appearances in final frontier
  final_frontier <- paths$frontiers[[length(paths$frontiers)]]

  var_counts <- table(unlist(lapply(final_frontier, function(m) m$vars)))

  # Get AIC of best model containing each variable
  best_aic_with_var <- sapply(names(stab$pi), function(v) {
    models_with_v <- which(sapply(final_frontier, function(m) v %in% m$vars))
    if (length(models_with_v) == 0) return(NA)
    min(sapply(final_frontier[models_with_v], function(m) m$aic))
  })

  # Create ranking data frame
  ranking <- data.frame(
    Variable = names(stab$pi),
    Stability = stab$pi,
    AppearanceCount = as.numeric(var_counts[names(stab$pi)]),
    BestAICwithVar = best_aic_with_var,
    stringsAsFactors = FALSE
  )

  # Replace NA with 0 for variables not appearing
  ranking$AppearanceCount[is.na(ranking$AppearanceCount)] <- 0

  # Compute composite importance score
  # Normalize each component to [0,1]
  ranking$StabilityNorm <- ranking$Stability
  ranking$CountNorm <- ranking$AppearanceCount / max(ranking$AppearanceCount, na.rm = TRUE)

  # For AIC: lower is better, so invert
  if (any(!is.na(ranking$BestAICwithVar))) {
    aic_range <- range(ranking$BestAICwithVar, na.rm = TRUE)
    ranking$AICNorm <- 1 - (ranking$BestAICwithVar - aic_range[1]) / diff(aic_range)
    ranking$AICNorm[is.na(ranking$AICNorm)] <- 0
  } else {
    ranking$AICNorm <- 0
  }

  # Composite importance (weighted average)
  ranking$ImportanceScore <- (0.5 * ranking$StabilityNorm +
                                0.3 * ranking$CountNorm +
                                0.2 * ranking$AICNorm)

  # Sort by importance
  ranking <- ranking[order(-ranking$ImportanceScore), ]
  rownames(ranking) <- NULL

  # Clean up intermediate columns
  ranking <- ranking[, c("Variable", "Stability", "AppearanceCount",
                         "BestAICwithVar", "ImportanceScore")]

  return(ranking)
}


#' Plot Variable Importance Ranking
#'
#' @description
#' Visualizes the variable importance ranking as a horizontal bar chart.
#'
#' @param ranking Output from variable_importance_ranking()
#' @param top_n Integer: show top N variables. Default: 15
#' @param col_bars Color gradient for bars. Can be:
#'   - A single color (e.g., "steelblue")
#'   - A vector of 2 colors for gradient (e.g., c("lightblue", "darkblue"))
#'   - A vector of colors matching number of bars
#'   Default: gradient from lightblue to darkblue
#' @param col_stability Color for stability points. Default: "red"
#'
#' @export
plot_variable_importance <- function(ranking, top_n = 15,
                                     col_bars = c("lightblue", "darkblue"),
                                     col_stability = "red") {

  # Limit to top N
  if (nrow(ranking) > top_n) {
    ranking <- ranking[1:top_n, ]
  }

  # Create color vector
  if (length(col_bars) == 1) {
    # Single color - use it for all bars
    bar_colors <- rep(col_bars, nrow(ranking))
  } else if (length(col_bars) == 2) {
    # Two colors - create gradient
    bar_colors <- colorRampPalette(col_bars)(nrow(ranking))
  } else {
    # Multiple colors provided - use as is or recycle
    bar_colors <- rep(col_bars, length.out = nrow(ranking))
  }

  # Create plot
  par(mar = c(4, 8, 4, 2))

  barplot(rev(ranking$ImportanceScore),
          names.arg = rev(ranking$Variable),
          horiz = TRUE,
          las = 1,
          main = "Variable Importance Ranking",
          xlab = "Composite Importance Score",
          col = rev(bar_colors),
          xlim = c(0, 1))

  grid(nx = NA, ny = NULL)

  # Add stability scores as points
  points(rev(ranking$Stability), 1:nrow(ranking),
         pch = 18, col = col_stability, cex = 1.5)

  legend("bottomright",
         legend = c("Composite importance", "Stability score"),
         fill = c(bar_colors[nrow(ranking)], NA),
         pch = c(NA, 18),
         col = c(NA, col_stability),
         bty = "n")
}
