## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(multipathaic)
library(lars)
set.seed(2025)

## ----load_data----------------------------------------------------------------
data(diabetes, package = "lars")
X_diabetes <- as.matrix(diabetes$x)
y <- diabetes$y

print(paste("Original dimensions:", nrow(X_diabetes), "observations,", ncol(X_diabetes), "predictors"))

## ----feature_engineering------------------------------------------------------
# Convert to data frame for model.matrix
X <- as.data.frame(X_diabetes)

# Create formula for second-order terms (interactions + quadratics)
formula_expanded <- as.formula(
  paste("~ (", paste(colnames(X), collapse = " + "), ")^2 + I(", 
        paste(colnames(X), "^2", collapse = ") + I("), ")")
)

# Generate expanded feature matrix
X_expanded <- as.data.frame(model.matrix(formula_expanded, data = X))
X_expanded <- X_expanded[, -1]  # remove intercept

# Clean column names to avoid formula issues
colnames(X_expanded) <- make.names(colnames(X_expanded), unique = TRUE)

print(paste("Initial expanded dimensions:", ncol(X_expanded), "predictors"))

# Limit to 60 predictors
if (ncol(X_expanded) > 60) {
  X_expanded <- X_expanded[, 1:60]
  print("Selected first 60 predictors for analysis")
}

print("  - Original features: 10")
print("  - Quadratic terms: 10")
print("  - Interaction terms: 40")
print("  - Total: 60 predictors")

# Standardize predictors
X <- as.matrix(scale(X_expanded))

print(paste("Final dimensions:", nrow(X), "observations ×", ncol(X), "predictors"))

## ----train_test---------------------------------------------------------------
# 80/20 train-test split
n <- nrow(X)
train_idx <- sample(1:n, size = floor(0.8 * n))

X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test <- X[-train_idx, ]
y_test <- y[-train_idx]

print(paste("Training set:", length(train_idx), "observations"))
print(paste("Test set:", length(y_test), "observations"))

## ----build_paths--------------------------------------------------------------
paths <- build_paths(
  X = X_train,
  y = y_train,
  family = "gaussian",
  K = min(ncol(X_train), 10),
  eps = 1e-6,
  delta = 2,
  L = 100,
  verbose = FALSE
)

print(paths)

## ----stability----------------------------------------------------------------
stab <- stability(
  X = X_train,
  y = y_train,
  family = "gaussian",
  B = 20,
  resample_type = "bootstrap",
  K = min(ncol(X_train), 10),
  eps = 1e-6,
  delta = 2,
  L = 50,
  verbose = FALSE
)

# Top 5 most stable predictors
head(sort(stab$pi, decreasing = TRUE), 5)

## ----plausible----------------------------------------------------------------
plaus <- plausible_models(
  path_result = paths,
  stability_result = stab,
  Delta = 4,
  tau = 0.5,
  remove_duplicates = TRUE,
  refit = FALSE,
  X = X_train,
  y = y_train
)

print(plaus)

## ----plot_aic, fig.width=7, fig.height=5--------------------------------------
plot_aic_by_step(paths)

## ----plot_stability, fig.width=7, fig.height=5--------------------------------
plot_stability(stab, threshold = 0.6)

## ----test_performance---------------------------------------------------------
# Get best model variables
selected_vars <- plaus$variables[[1]]
if (is.character(selected_vars) && length(selected_vars) == 1) {
  selected_vars <- trimws(strsplit(selected_vars, ",")[[1]])
}

# Fit model on training data
X_train_sel <- X_train[, selected_vars, drop = FALSE]
train_data <- data.frame(y = y_train, X_train_sel)
model <- lm(y ~ ., data = train_data)

# Test predictions
X_test_sel <- X_test[, selected_vars, drop = FALSE]
test_data <- data.frame(X_test_sel)
y_pred <- predict(model, newdata = test_data)

# Calculate metrics
rmse_test <- sqrt(mean((y_test - y_pred)^2))
r2_test <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

print(paste("Test RMSE:", round(rmse_test, 2)))
print(paste("Test R²:", round(r2_test, 4)))
print(paste("Model size:", length(selected_vars), "predictors"))

