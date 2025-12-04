## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(multipathaic)
library(mlbench)
set.seed(2025)

## ----load_data----------------------------------------------------------------
data(BreastCancer, package = "mlbench")

# Remove ID and missing values
bc_data <- na.omit(BreastCancer[, -1])

# Convert to numeric (except Class)
for (col in names(bc_data)[-ncol(bc_data)]) {
  bc_data[[col]] <- as.numeric(as.character(bc_data[[col]]))
}

# Binary outcome: malignant = 1, benign = 0
y <- as.numeric(bc_data$Class == "malignant")
X <- as.matrix(bc_data[, -ncol(bc_data)])
X <- scale(X)

print(paste("Data dimensions:", nrow(X), "observations,", ncol(X), "predictors"))
print(paste("Malignant cases:", sum(y), "(", round(100*mean(y), 1), "%)"))

## ----train_test---------------------------------------------------------------
# Stratified 80/20 split
n <- nrow(X)
mal_idx <- which(y == 1)
ben_idx <- which(y == 0)

train_mal <- sample(mal_idx, floor(0.8 * length(mal_idx)))
train_ben <- sample(ben_idx, floor(0.8 * length(ben_idx)))
train_idx <- c(train_mal, train_ben)

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
  family = "binomial",
  K = 6,
  eps = 1e-6,
  delta = 2,
  L = 30,
  verbose = FALSE
)

print(paths)

## ----stability----------------------------------------------------------------
stab <- stability(
  X = X_train,
  y = y_train,
  family = "binomial",
  B = 20,
  resample_type = "bootstrap",
  K = 6,
  eps = 1e-6,
  delta = 2,
  L = 30,
  verbose = FALSE
)

# Top 5 most stable predictors
head(sort(stab$pi, decreasing = TRUE), 5)

## ----plausible----------------------------------------------------------------
plaus <- plausible_models(
  path_result = paths,
  stability_result = stab,
  Delta = 3,
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
plot_stability(stab, threshold = 0.5)

## ----test_performance---------------------------------------------------------
# Get best model variables
selected_vars <- plaus$variables[[1]]
if (is.character(selected_vars) && length(selected_vars) == 1) {
  selected_vars <- trimws(strsplit(selected_vars, ",")[[1]])
}

# Fit model on training data
X_train_sel <- X_train[, selected_vars, drop = FALSE]
train_data <- data.frame(y = y_train, X_train_sel)
model <- glm(y ~ ., data = train_data, family = binomial)

# Test predictions
X_test_sel <- X_test[, selected_vars, drop = FALSE]
test_data <- data.frame(X_test_sel)
pred_prob <- predict(model, newdata = test_data, type = "response")
pred_class <- as.numeric(pred_prob > 0.5)

# Confusion matrix
cm <- table(Actual = y_test, Predicted = pred_class)
print(cm)

# Calculate metrics
accuracy <- sum(diag(cm)) / sum(cm)
sensitivity <- cm[2, 2] / sum(cm[2, ])
specificity <- cm[1, 1] / sum(cm[1, ])

print(paste("Test Accuracy:", round(100 * accuracy, 1), "%"))
print(paste("Sensitivity:", round(100 * sensitivity, 1), "%"))
print(paste("Specificity:", round(100 * specificity, 1), "%"))
print(paste("Model size:", length(selected_vars), "predictors"))

