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