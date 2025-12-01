# ==============================================================================
# COMPLETE TEST - multipathaic with Built-in R Datasets
# ==============================================================================

library(multipathaic)
set.seed(123)

# ==============================================================================
# TEST 1: MTCARS - LINEAR REGRESSION (Predict MPG)
# ==============================================================================

data(mtcars)
X_cars <- as.matrix(mtcars[, c("cyl", "disp", "hp", "drat", "wt", "qsec")])
y_cars <- mtcars$mpg

# Run algorithms
paths_cars <- build_paths(X_cars, y_cars, family = "gaussian", K = 5, delta = 2, verbose = FALSE)
stab_cars <- stability(X_cars, y_cars, family = "gaussian", B = 40, K = 5, verbose = FALSE)
plausible_cars <- plausible_models(paths_cars, stab_cars, Delta = 3, tau = 0.5, refit = TRUE, X = X_cars, y = y_cars)

# Results
print(paths_cars)
print(stab_cars)
print(plausible_cars)

# Helper functions
all_models_cars <- extract_all_models(paths_cars)
importance_cars <- variable_importance_ranking(paths_cars, stab_cars)
print(importance_cars)

# Plots
plot_stability(stab_cars, threshold = 0.5, top_n = 6)
plot_aic_by_step(paths_cars)
plot_model_tree(paths_cars, max_models = 20)
if (nrow(plausible_cars) > 1) plot_variable_heatmap(plausible_cars)
plot_model_dashboard(paths_cars, stab_cars, plausible_cars)
plot_stability_distribution(stab_cars, top_n = 5)
plot_variable_importance(importance_cars, top_n = 6)


# ==============================================================================
# TEST 2: MTCARS - LOGISTIC REGRESSION (Predict Transmission Type)
# ==============================================================================

# Use mtcars to predict transmission type (am: 0=automatic, 1=manual)
data(mtcars)
X_trans <- as.matrix(mtcars[, c("mpg", "cyl", "disp", "hp", "wt", "qsec")])
y_trans <- mtcars$am  # 0 = automatic, 1 = manual

# Run algorithms
paths_trans <- build_paths(X_trans, y_trans, family = "binomial", K = 4, delta = 2, verbose = FALSE)
stab_trans <- stability(X_trans, y_trans, family = "binomial", B = 30, K = 4, verbose = FALSE)
plausible_trans <- plausible_models(paths_trans, stab_trans, Delta = 2, tau = 0.5, refit = TRUE, X = X_trans, y = y_trans)

# Results
print(paths_trans)
print(stab_trans)
print(plausible_trans)

# Classification metrics - THIS WILL WORK NOW!
if (nrow(plausible_trans) > 0) {
  metrics_trans <- confusion_metrics(plausible_trans$fitted_model[[1]], X_trans, y_trans)
  print(metrics_trans$confusion_matrix)
  print(metrics_trans$metrics)
}

# Plots
plot_stability(stab_trans, threshold = 0.5)
plot_model_tree(paths_trans, max_models = 15)


# ==============================================================================
# TEST 3: CUSTOM COLORS
# ==============================================================================

# Custom colors
plot_stability(stab_cars, top_n = 6, col_stable = "darkgreen", col_unstable = "orange", col_threshold = "purple")
plot_aic_by_step(paths_cars, col_box = "lightyellow", col_border = "darkorange", col_median = "darkblue", col_best = "red")
plot_model_tree(paths_cars, max_models = 20, col_models = "lightgreen", col_best = "purple", col_line = "orange")
if (nrow(plausible_cars) > 1) plot_variable_heatmap(plausible_cars, col_palette = c("white", "yellow", "orange", "red"))
plot_model_dashboard(paths_cars, stab_cars, plausible_cars, col_stability = "purple", col_size = "green")
plot_variable_importance(importance_cars, top_n = 6, col_bars = c("yellow", "red"), col_stability = "blue")


# ==============================================================================
# SUMMARY
# ==============================================================================

data.frame(
  Dataset = c("mtcars-MPG", "mtcars-Transmission"),
  Task = c("Predict MPG", "Predict Transmission"),
  Family = c("Linear", "Logistic"),
  ModelsExplored = c(nrow(all_models_cars), nrow(extract_all_models(paths_trans))),
  PlausibleModels = c(nrow(plausible_cars), nrow(plausible_trans)),
  MostStable = c(names(which.max(stab_cars$pi)), names(which.max(stab_trans$pi))),
  Stability = c(max(stab_cars$pi), max(stab_trans$pi))
)
