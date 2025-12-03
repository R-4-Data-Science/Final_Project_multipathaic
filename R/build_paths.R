#' Multi-Path Forward Selection Using AIC
#'
#' @description
#' This function implements a multi-path forward selection algorithm that explores
#' multiple promising model paths simultaneously, rather than following just a single
#' "best" sequence. At each step, it adds predictors that improve AIC and keeps all
#' models that perform nearly as well as the best one.
#'
#' @details
#' The algorithm works as follows:
#' \enumerate{
#'   \item Start with the empty model (intercept only)
#'   \item At each step k = 1 to K:
#'     \itemize{
#'       \item From every current model (parent), try adding each unused variable (children)
#'       \item Compute AIC for each candidate child
#'       \item For each parent, keep all children within delta of that parent's best AIC
#'       \item Only keep children if they improve parent AIC by at least eps
#'       \item Deduplicate and keep best L models if there are too many
#'     }
#'   \item Return the collection of models at each step (a tree of paths)
#' }
#'
#' @param X Matrix or data frame of predictors (n x p)
#' @param y Response vector (length n)
#' @param family Character string: "gaussian" for linear regression or "binomial" for logistic
#' @param K Integer: maximum number of steps (model size). Default: min(ncol(X), 10)
#' @param eps Numeric: minimum AIC improvement required to continue expanding. Default: 1e-6
#' @param delta Numeric: AIC tolerance for keeping near-ties. If delta=0, only keep single best.
#'   Default: 1
#' @param L Integer: maximum number of models to keep per step. Default: 50
#' @param verbose Logical: print progress messages? Default: FALSE
#'
#' @return A list with three components:
#'   \item{frontiers}{A list of length K, where each element contains the models at that step}
#'   \item{aic_by_model}{A named list of AIC values for all models}
#'   \item{meta}{Metadata including parameters used}
#' 
#' @author Soroosh Alavi
#' @export
#' @examples
#' \dontrun{
#' # Linear regression example
#' set.seed(123)
#' n <- 100
#' p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("x", 1:p)
#' y <- X[,1] + 0.5 * X[,2] + rnorm(n)
#'
#' # Run multi-path search
#' result <- build_paths(X, y, family = "gaussian", K = 3, delta = 1)
#' }
build_paths <- function(X, y,
                        family = c("gaussian", "binomial"),
                        K = NULL,
                        eps = 1e-6,
                        delta = 1,
                        L = 50,
                        verbose = FALSE) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  family <- match.arg(family)

  # Convert X to matrix if it's a data frame
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  # Check dimensions
  if (!is.matrix(X)) {
    stop("X must be a matrix or data frame")
  }

  n <- nrow(X)
  p <- ncol(X)

  if (length(y) != n) {
    stop("Length of y must equal number of rows in X")
  }

  # Set default K if not provided
  if (is.null(K)) {
    K <- min(p, 10)
  }

  if (K > p) {
    warning("K is larger than number of predictors. Setting K = p")
    K <- p
  }

  # Get variable names
  var_names <- colnames(X)
  if (is.null(var_names)) {
    var_names <- paste0("V", 1:p)
    colnames(X) <- var_names
  }

  if (verbose) {
    cat(sprintf("Starting multi-path search with:\n"))
    cat(sprintf("  n = %d observations\n", n))
    cat(sprintf("  p = %d predictors\n", p))
    cat(sprintf("  K = %d maximum steps\n", K))
    cat(sprintf("  family = %s\n", family))
    cat(sprintf("  delta = %.4f\n", delta))
    cat(sprintf("  eps = %.6f\n", eps))
    cat(sprintf("  L = %d\n\n", L))
  }

  # ============================================================================
  # HELPER FUNCTION: Fit a model and return AIC
  # ============================================================================

  fit_and_get_aic <- function(vars_in_model) {
    # If empty model (only intercept)
    if (length(vars_in_model) == 0) {
      if (family == "gaussian") {
        fit <- lm(y ~ 1)
      } else {
        fit <- glm(y ~ 1, family = binomial)
      }
    } else {
      # Model with selected variables
      X_sub <- X[, vars_in_model, drop = FALSE]
      if (family == "gaussian") {
        fit <- lm(y ~ X_sub)
      } else {
        fit <- glm(y ~ X_sub, family = binomial)
      }
    }
    return(AIC(fit))
  }

  # ============================================================================
  # HELPER FUNCTION: Create a unique model identifier
  # ============================================================================

  model_to_string <- function(vars) {
    if (length(vars) == 0) return("EMPTY")
    paste(sort(vars), collapse = "+")
  }

  # ============================================================================
  # INITIALIZATION: Start with empty model
  # ============================================================================

  # Store all models across all steps
  frontiers <- vector("list", K)
  aic_storage <- list()

  # Step 0: Empty model (intercept only)
  empty_model <- list(
    vars = character(0),
    aic = fit_and_get_aic(character(0)),
    model_id = "EMPTY"
  )

  # Current models being explored
  current_models <- list(empty_model)

  if (verbose) {
    cat(sprintf("Step 0: Empty model, AIC = %.2f\n\n", empty_model$aic))
  }

  # ============================================================================
  # MAIN LOOP: Build paths step by step
  # ============================================================================

  for (k in 1:K) {

    if (verbose) {
      cat(sprintf("========== Step %d ==========\n", k))
      cat(sprintf("Starting with %d parent models\n", length(current_models)))
    }

    # Container for all candidate children in this step
    all_children <- list()

    # --------------------------------------------------------------------
    # For each parent model, generate children by adding one variable
    # --------------------------------------------------------------------

    for (parent_idx in seq_along(current_models)) {

      parent <- current_models[[parent_idx]]
      parent_vars <- parent$vars
      parent_aic <- parent$aic

      # Find variables not yet in this parent model
      available_vars <- setdiff(var_names, parent_vars)

      if (length(available_vars) == 0) {
        # No more variables to add to this parent
        next
      }

      # Try adding each available variable
      children_for_this_parent <- list()

      for (new_var in available_vars) {
        # Create child model
        child_vars <- c(parent_vars, new_var)
        child_aic <- fit_and_get_aic(child_vars)
        child_id <- model_to_string(child_vars)

        children_for_this_parent[[length(children_for_this_parent) + 1]] <- list(
          vars = child_vars,
          aic = child_aic,
          model_id = child_id,
          parent_id = parent$model_id
        )
      }

      # --------------------------------------------------------------------
      # Filter children: keep those within delta of parent's best child
      # AND that improve parent AIC by at least eps
      # --------------------------------------------------------------------

      if (length(children_for_this_parent) > 0) {

        # Find best child AIC for this parent
        child_aics <- sapply(children_for_this_parent, function(x) x$aic)
        best_child_aic <- min(child_aics)

        # Check if best child improves parent AIC by at least eps
        improvement <- parent_aic - best_child_aic

        if (improvement >= eps) {
          # Keep children within delta of best child AIC
          keep_indices <- which(child_aics <= best_child_aic + delta)

          # Add kept children to all_children
          for (idx in keep_indices) {
            all_children[[length(all_children) + 1]] <- children_for_this_parent[[idx]]
          }
        }
      }
    }

    # --------------------------------------------------------------------
    # Check if we found any valid children
    # --------------------------------------------------------------------

    if (length(all_children) == 0) {
      if (verbose) {
        cat("No children met the improvement criteria. Stopping early.\n")
      }
      # Trim frontiers to actual steps taken
      frontiers <- frontiers[1:(k-1)]
      break
    }

    # --------------------------------------------------------------------
    # Deduplicate: Remove models with identical variable sets
    # --------------------------------------------------------------------

    unique_model_ids <- unique(sapply(all_children, function(x) x$model_id))

    unique_children <- list()
    for (uid in unique_model_ids) {
      # Find first occurrence of this model
      idx <- which(sapply(all_children, function(x) x$model_id == uid))[1]
      unique_children[[length(unique_children) + 1]] <- all_children[[idx]]
    }

    if (verbose) {
      cat(sprintf("Generated %d children, %d unique\n",
                  length(all_children), length(unique_children)))
    }

    # --------------------------------------------------------------------
    # If too many models, keep only the best L by AIC
    # --------------------------------------------------------------------

    if (length(unique_children) > L) {
      # Sort by AIC
      aics <- sapply(unique_children, function(x) x$aic)
      sorted_indices <- order(aics)
      unique_children <- unique_children[sorted_indices[1:L]]

      if (verbose) {
        cat(sprintf("Limiting to best L = %d models\n", L))
      }
    }

    # --------------------------------------------------------------------
    # Store results for this step
    # --------------------------------------------------------------------

    frontiers[[k]] <- unique_children
    current_models <- unique_children

    # Store AICs
    for (child in unique_children) {
      aic_storage[[child$model_id]] <- child$aic
    }

    if (verbose) {
      best_aic <- min(sapply(unique_children, function(x) x$aic))
      cat(sprintf("Step %d complete: %d models kept, best AIC = %.2f\n\n",
                  k, length(unique_children), best_aic))
    }
  }

  # ============================================================================
  # RETURN RESULTS
  # ============================================================================

  meta <- list(
    n = n,
    p = p,
    K = length(frontiers),  # Actual steps taken
    family = family,
    eps = eps,
    delta = delta,
    L = L,
    var_names = var_names
  )

  result <- list(
    frontiers = frontiers,
    aic_by_model = aic_storage,
    meta = meta
  )

  class(result) <- c("multipath", "list")

  return(result)
}


#' Print method for multipath objects
#'
#' @param x A multipath object from build_paths()
#' @param ... Additional arguments (not used)
#' @export
print.multipath <- function(x, ...) {
  cat("Multi-Path Forward Selection Result\n")
  cat("====================================\n")
  cat(sprintf("Family: %s\n", x$meta$family))
  cat(sprintf("Sample size: %d\n", x$meta$n))
  cat(sprintf("Number of predictors: %d\n", x$meta$p))
  cat(sprintf("Steps completed: %d\n", x$meta$K))
  cat(sprintf("Parameters: eps=%.2e, delta=%.2f, L=%d\n",
              x$meta$eps, x$meta$delta, x$meta$L))
  cat("\nModels per step:\n")
  for (k in seq_along(x$frontiers)) {
    cat(sprintf("  Step %d: %d models\n", k, length(x$frontiers[[k]])))
  }

  # Show best model at final step
  if (length(x$frontiers) > 0) {
    last_step <- x$frontiers[[length(x$frontiers)]]
    best_model <- last_step[[which.min(sapply(last_step, function(m) m$aic))]]
    cat(sprintf("\nBest model at final step (AIC = %.2f):\n", best_model$aic))
    cat(sprintf("  Variables: %s\n", paste(best_model$vars, collapse = ", ")))
  }
}

