#' Select Plausible Models Based on AIC and Stability
#'
#' @description
#' This function filters the models from multi-path search to identify "plausible"
#' models: those that are both statistically sound (AIC close to the best) and
#' reliable (built from stable predictors).
#'
#' @details
#' The algorithm works as follows:
#' \enumerate{
#'   \item Find the minimum AIC across all models in the path forest
#'   \item Keep models with AIC ≤ AIC_min + Delta (within Delta of the best)
#'   \item For each kept model S, compute average stability: π(S) = mean(π_j for j in S)
#'   \item Keep only models with π(S) ≥ tau (average stability threshold)
#'   \item Optionally remove near-duplicates using Jaccard similarity
#' }
#'
#' @param path_result Output from build_paths()
#' @param stability_result Output from stability() (or NULL to skip stability filtering)
#' @param Delta Numeric: AIC tolerance. Keep models with AIC ≤ AIC_min + Delta. Default: 2
#' @param tau Numeric: minimum average stability threshold (0 to 1). Default: 0.6
#' @param remove_duplicates Logical: remove near-duplicate models? Default: TRUE
#' @param jaccard_threshold Numeric: Jaccard similarity threshold for duplicate detection.
#'   Models with similarity > threshold are considered duplicates. Default: 0.9
#' @param refit Logical: refit final models on full data? Default: FALSE
#' @param X Matrix of predictors (needed if refit = TRUE)
#' @param y Response vector (needed if refit = TRUE)
#'
#' @return A data frame where each row is a plausible model with columns:
#'   \item{model_id}{String identifier for the model}
#'   \item{size}{Number of predictors in the model}
#'   \item{variables}{Character string of variables (comma-separated)}
#'   \item{aic}{AIC value}
#'   \item{delta_aic}{Difference from minimum AIC}
#'   \item{avg_stability}{Average stability score (if stability_result provided)}
#'   \item{fitted_model}{Fitted model object (if refit = TRUE)}
#'
#' @export
#' @examples
#' \dontrun{
#' # Example workflow
#' paths <- build_paths(X, y, family = "gaussian", K = 5)
#' stab <- stability(X, y, family = "gaussian", B = 50)
#' plausible <- plausible_models(paths, stab, Delta = 2, tau = 0.6)
#' print(plausible)
#' }
plausible_models <- function(path_result,
                             stability_result = NULL,
                             Delta = 2,
                             tau = 0.6,
                             remove_duplicates = TRUE,
                             jaccard_threshold = 0.9,
                             refit = FALSE,
                             X = NULL,
                             y = NULL) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  if (!inherits(path_result, "multipath")) {
    stop("path_result must be output from build_paths()")
  }

  if (!is.null(stability_result) && !inherits(stability_result, "stability")) {
    stop("stability_result must be output from stability() or NULL")
  }

  if (refit && (is.null(X) || is.null(y))) {
    stop("X and y must be provided if refit = TRUE")
  }

  # ============================================================================
  # EXTRACT ALL UNIQUE MODELS FROM PATH FOREST
  # ============================================================================

  all_models <- list()

  for (step in path_result$frontiers) {
    for (model in step) {
      all_models[[model$model_id]] <- model
    }
  }

  if (length(all_models) == 0) {
    stop("No models found in path_result")
  }

  # ============================================================================
  # FILTER BY AIC: Keep models within Delta of minimum
  # ============================================================================

  # Get all AIC values
  all_aics <- sapply(all_models, function(m) m$aic)
  aic_min <- min(all_aics)

  # Filter: keep models with AIC <= aic_min + Delta
  aic_filtered <- all_models[all_aics <= aic_min + Delta]

  cat(sprintf("AIC Filtering:\n"))
  cat(sprintf("  Total models: %d\n", length(all_models)))
  cat(sprintf("  Minimum AIC: %.2f\n", aic_min))
  cat(sprintf("  Delta: %.2f\n", Delta))
  cat(sprintf("  Models within Delta: %d\n", length(aic_filtered)))

  # ============================================================================
  # FILTER BY STABILITY: Keep models with average stability >= tau
  # ============================================================================

  if (!is.null(stability_result)) {

    pi <- stability_result$pi

    # Compute average stability for each model
    avg_stabilities <- sapply(aic_filtered, function(model) {
      if (length(model$vars) == 0) {
        return(0)  # Empty model has no stability
      }
      mean(pi[model$vars])
    })

    # Filter: keep models with avg_stability >= tau
    stability_filtered <- aic_filtered[avg_stabilities >= tau]

    cat(sprintf("\nStability Filtering:\n"))
    cat(sprintf("  Tau threshold: %.2f\n", tau))
    cat(sprintf("  Models with avg stability >= tau: %d\n", length(stability_filtered)))

    final_models <- stability_filtered

  } else {
    cat("\nNo stability filtering (stability_result = NULL)\n")
    final_models <- aic_filtered
  }

  # ============================================================================
  # REMOVE NEAR-DUPLICATES USING JACCARD SIMILARITY
  # ============================================================================

  if (remove_duplicates && length(final_models) > 1) {

    # Compute Jaccard similarity for all pairs
    model_list <- final_models
    n_models <- length(model_list)

    keep <- rep(TRUE, n_models)

    for (i in 1:(n_models - 1)) {
      if (!keep[i]) next

      vars_i <- model_list[[i]]$vars

      for (j in (i + 1):n_models) {
        if (!keep[j]) next

        vars_j <- model_list[[j]]$vars

        # Compute Jaccard similarity
        intersection <- length(intersect(vars_i, vars_j))
        union <- length(union(vars_i, vars_j))

        if (union > 0) {
          jaccard <- intersection / union

          # If similarity exceeds threshold, remove the model with worse AIC
          if (jaccard > jaccard_threshold) {
            if (model_list[[i]]$aic <= model_list[[j]]$aic) {
              keep[j] <- FALSE
            } else {
              keep[i] <- FALSE
              break
            }
          }
        }
      }
    }

    final_models <- model_list[keep]

    cat(sprintf("\nDuplicate Removal:\n"))
    cat(sprintf("  Jaccard threshold: %.2f\n", jaccard_threshold))
    cat(sprintf("  Models after removing duplicates: %d\n", length(final_models)))
  }

  # ============================================================================
  # CONVERT TO DATA FRAME
  # ============================================================================

  if (length(final_models) == 0) {
    warning("No models passed all filters!")
    return(data.frame())
  }

  result_df <- data.frame(
    model_id = character(length(final_models)),
    size = integer(length(final_models)),
    variables = character(length(final_models)),
    aic = numeric(length(final_models)),
    delta_aic = numeric(length(final_models)),
    stringsAsFactors = FALSE
  )

  # Add stability column if applicable
  if (!is.null(stability_result)) {
    result_df$avg_stability <- numeric(length(final_models))
  }

  for (i in seq_along(final_models)) {
    model <- final_models[[i]]

    result_df$model_id[i] <- model$model_id
    result_df$size[i] <- length(model$vars)
    result_df$variables[i] <- ifelse(length(model$vars) == 0,
                                     "(intercept only)",
                                     paste(model$vars, collapse = ", "))
    result_df$aic[i] <- model$aic
    result_df$delta_aic[i] <- model$aic - aic_min

    if (!is.null(stability_result)) {
      if (length(model$vars) == 0) {
        result_df$avg_stability[i] <- 0
      } else {
        result_df$avg_stability[i] <- mean(stability_result$pi[model$vars])
      }
    }
  }

  # Sort by AIC
  result_df <- result_df[order(result_df$aic), ]
  rownames(result_df) <- NULL

  # ============================================================================
  # REFIT MODELS IF REQUESTED
  # ============================================================================

  if (refit) {
    cat("\nRefitting models on full data...\n")

    if (is.data.frame(X)) {
      X <- as.matrix(X)
    }

    family <- path_result$meta$family

    fitted_models <- vector("list", nrow(result_df))

    for (i in 1:nrow(result_df)) {
      vars <- final_models[[i]]$vars

      if (length(vars) == 0) {
        # Intercept-only model
        if (family == "gaussian") {
          fitted_models[[i]] <- lm(y ~ 1)
        } else {
          fitted_models[[i]] <- glm(y ~ 1, family = binomial)
        }
      } else {
        X_sub <- X[, vars, drop = FALSE]
        if (family == "gaussian") {
          fitted_models[[i]] <- lm(y ~ X_sub)
        } else {
          fitted_models[[i]] <- glm(y ~ X_sub, family = binomial)
        }
      }
    }

    result_df$fitted_model <- fitted_models
  }

  # ============================================================================
  # FINAL OUTPUT
  # ============================================================================

  cat("\n==============================================\n")
  cat(sprintf("Final Plausible Model Set: %d models\n", nrow(result_df)))
  cat("==============================================\n\n")

  class(result_df) <- c("plausible_models", "data.frame")

  return(result_df)
}


#' Print method for plausible_models objects
#'
#' @param x A plausible_models data frame
#' @param ... Additional arguments (not used)
#' @export
print.plausible_models <- function(x, ...) {
  cat("Plausible Models\n")
  cat("================\n")
  cat(sprintf("Number of models: %d\n\n", nrow(x)))

  # Print as regular data frame but without fitted_model column if present
  display_cols <- setdiff(names(x), "fitted_model")
  print(as.data.frame(x[, display_cols]))
}
