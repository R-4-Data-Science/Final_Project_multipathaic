#' Compute Variable Stability Through Resampling
#'
#' @description
#' This function estimates the stability of predictor selection by repeating the
#' multi-path search on resampled versions of the data. For each resample, it
#' computes the proportion of models that include each variable. These proportions
#' are then averaged across all resamples to produce stability scores.
#'
#' @details
#' The algorithm works as follows:
#' \enumerate{
#'   \item For b = 1 to B:
#'     \itemize{
#'       \item Draw a bootstrap sample or subsample of the data
#'       \item Run build_paths() on that resample
#'       \item For each predictor j, compute z_j^(b) = proportion of models containing j
#'     }
#'   \item Compute stability scores: pi_j = (1/B) * Sum_b z_j^(b)
#' }
#'
#' A stability score close to 1 means the variable is almost always selected;
#' a score near 0 means it's rarely used.
#'
#' @param X Matrix or data frame of predictors (n x p)
#' @param y Response vector (length n)
#' @param family Character string: "gaussian" or "binomial"
#' @param B Integer: number of resamples. Default: 50
#' @param resample_type Character: "bootstrap" (sample with replacement) or
#'   "subsample" (sample without replacement). Default: "bootstrap"
#' @param m Integer: subsample size if resample_type = "subsample".
#'   Default: ceiling(sqrt(n))
#' @param K Integer: maximum steps for build_paths(). Default: NULL (uses build_paths default)
#' @param eps Numeric: minimum AIC improvement. Default: 1e-6
#' @param delta Numeric: AIC tolerance for near-ties. Default: 1
#' @param L Integer: max models per step. Default: 50
#' @param verbose Logical: print progress? Default: FALSE
#' @param parallel Logical: use parallel processing? Default: FALSE
#' @param ncores Integer: number of cores if parallel = TRUE. Default: 2
#'
#' @return A list with components:
#'   \item{pi}{Named numeric vector of stability scores (length p)}
#'   \item{z_matrix}{Matrix of per-resample proportions (B x p)}
#'   \item{meta}{Metadata including parameters and variable names}
#'
#' @export
#' @examples
#' \dontrun{
#' # Example with simulated data
#' set.seed(123)
#' n <- 100
#' p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' colnames(X) <- paste0("x", 1:p)
#' y <- X[,1] + 0.5 * X[,2] + rnorm(n)
#'
#' # Compute stability
#' stab <- stability(X, y, family = "gaussian", B = 20, verbose = TRUE)
#' print(stab$pi)
#' }
stability <- function(X, y,
                      family = c("gaussian", "binomial"),
                      B = 50,
                      resample_type = c("bootstrap", "subsample"),
                      m = NULL,
                      K = NULL,
                      eps = 1e-6,
                      delta = 1,
                      L = 50,
                      verbose = FALSE,
                      parallel = FALSE,
                      ncores = 2) {

  # ============================================================================
  # INPUT VALIDATION
  # ============================================================================

  family <- match.arg(family)
  resample_type <- match.arg(resample_type)

  # Convert X to matrix if needed
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  n <- nrow(X)
  p <- ncol(X)

  # Get variable names
  var_names <- colnames(X)
  if (is.null(var_names)) {
    var_names <- paste0("V", 1:p)
    colnames(X) <- var_names
  }

  # Set default subsample size if needed
  if (resample_type == "subsample" && is.null(m)) {
    m <- ceiling(sqrt(n))
  }

  if (verbose) {
    cat("==============================================\n")
    cat("Starting Stability Analysis via Resampling\n")
    cat("==============================================\n")
    cat(sprintf("Number of resamples: B = %d\n", B))
    cat(sprintf("Resample type: %s\n", resample_type))
    if (resample_type == "subsample") {
      cat(sprintf("Subsample size: m = %d (out of n = %d)\n", m, n))
    }
    cat(sprintf("Family: %s\n", family))
    cat("\n")
  }

  # ============================================================================
  # HELPER FUNCTION: Compute proportion of models containing each variable
  # ============================================================================

  compute_proportions <- function(path_result) {
    # Initialize proportion vector
    proportions <- setNames(rep(0, p), var_names)

    # Count total models across all steps
    total_models <- 0

    # Count how many times each variable appears
    var_counts <- setNames(rep(0, p), var_names)

    for (step_models in path_result$frontiers) {
      for (model in step_models) {
        total_models <- total_models + 1
        # Increment count for each variable in this model
        for (v in model$vars) {
          var_counts[v] <- var_counts[v] + 1
        }
      }
    }

    # Compute proportions
    if (total_models > 0) {
      proportions <- var_counts / total_models
    }

    return(proportions)
  }

  # ============================================================================
  # HELPER FUNCTION: Process a single resample
  # ============================================================================

  process_resample <- function(b) {
    # Draw resample
    if (resample_type == "bootstrap") {
      # Sample with replacement
      resample_idx <- sample(1:n, size = n, replace = TRUE)
    } else {
      # Subsample without replacement
      resample_idx <- sample(1:n, size = m, replace = FALSE)
    }

    X_resample <- X[resample_idx, , drop = FALSE]
    y_resample <- y[resample_idx]

    # Run multi-path search on this resample
    # Suppress warnings about convergence issues in resamples
    path_result <- tryCatch({
      suppressWarnings(
        build_paths(X_resample, y_resample,
                    family = family,
                    K = K,
                    eps = eps,
                    delta = delta,
                    L = L,
                    verbose = FALSE)
      )
    }, error = function(e) {
      # If this resample fails, return NULL
      return(NULL)
    })

    if (is.null(path_result)) {
      # Return zero proportions if resample failed
      return(setNames(rep(0, p), var_names))
    }

    # Compute proportions for this resample
    z_b <- compute_proportions(path_result)

    return(z_b)
  }

  # ============================================================================
  # RUN RESAMPLING (SEQUENTIAL OR PARALLEL)
  # ============================================================================

  if (parallel) {
    # Parallel processing
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("parallel package not available. Using sequential processing.")
      parallel <- FALSE
    }
  }

  if (parallel) {
    # Use parallel processing
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))

    # Export necessary objects to cluster
    parallel::clusterExport(cl,
                            varlist = c("X", "y", "n", "p", "m", "var_names",
                                        "family", "K", "eps", "delta", "L",
                                        "resample_type", "build_paths",
                                        "compute_proportions"),
                            envir = environment())

    if (verbose) {
      cat(sprintf("Running %d resamples in parallel using %d cores...\n", B, ncores))
    }

    # Run resampling in parallel
    z_list <- parallel::parLapply(cl, 1:B, process_resample)

  } else {
    # Sequential processing
    z_list <- vector("list", B)

    for (b in 1:B) {
      if (verbose && (b %% 10 == 0 || b == 1)) {
        cat(sprintf("  Resample %d of %d...\n", b, B))
      }

      z_list[[b]] <- process_resample(b)
    }
  }

  # ============================================================================
  # AGGREGATE RESULTS ACROSS RESAMPLES
  # ============================================================================

  # Convert list to matrix (B x p)
  z_matrix <- do.call(rbind, z_list)
  rownames(z_matrix) <- paste0("resample_", 1:B)
  colnames(z_matrix) <- var_names

  # Compute average stability scores
  pi <- colMeans(z_matrix)

  if (verbose) {
    cat("\n==============================================\n")
    cat("Stability Analysis Complete\n")
    cat("==============================================\n")
    cat("Stability scores (pi_j):\n")
    print(round(pi, 3))
    cat("\n")
  }

  # ============================================================================
  # RETURN RESULTS
  # ============================================================================

  meta <- list(
    n = n,
    p = p,
    var_names = var_names,
    B = B,
    resample_type = resample_type,
    m = if(resample_type == "subsample") m else NULL,
    family = family,
    K = K,
    eps = eps,
    delta = delta,
    L = L
  )

  result <- list(
    pi = pi,
    z_matrix = z_matrix,
    meta = meta
  )

  class(result) <- c("stability", "list")

  return(result)
}


#' Print method for stability objects
#'
#' @param x A stability object from stability()
#' @param ... Additional arguments (not used)
#' @export
print.stability <- function(x, ...) {
  cat("Variable Stability Analysis\n")
  cat("============================\n")
  cat(sprintf("Number of resamples: B = %d\n", x$meta$B))
  cat(sprintf("Resample type: %s\n", x$meta$resample_type))
  cat(sprintf("Family: %s\n", x$meta$family))
  cat("\nStability Scores (pi_j):\n")
  print(round(x$pi, 3))
  cat("\nInterpretation:\n")
  cat("  Values close to 1: variable almost always selected\n")
  cat("  Values close to 0: variable rarely selected\n")
}
