# multipathaic

Multi-Path Forward Selection with AIC and Stability Analysis

## Overview

`multipathaic` implements a novel approach to model selection that explores multiple promising paths through the model space, rather than following a single greedy sequence. The package provides:

- **Multi-path forward selection** based on AIC
- **Stability analysis** via bootstrap resampling
- **Plausible model identification** combining AIC and stability criteria
- **Advanced visualizations** including model trees, heatmaps, and importance rankings

## Installation

Install from GitHub:
```r
# Install remotes if needed
install.packages("remotes")

# Install multipathaic
remotes::install_github("R-4-Data-Science/multipathaic")
```

## Quick Start
```r
library(multipathaic)

# Simulate data
set.seed(123)
n <- 100
p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("x", 1:p)
y <- X[,1] + 0.5 * X[,2] + rnorm(n)

# Step 1: Multi-path forward selection
paths <- build_paths(X, y, family = "gaussian", K = 5, delta = 1.5)

# Step 2: Stability analysis
stab <- stability(X, y, family = "gaussian", B = 50)

# Step 3: Select plausible models
plausible <- plausible_models(paths, stab, Delta = 2, tau = 0.6)

# Visualize results
plot_model_tree(paths)
plot_stability(stab)
plot_variable_heatmap(plausible)
```

## Main Functions

### Core Algorithms

- `build_paths()` - Multi path forward selection
- `stability()` - Bootstrap stability analysis
- `plausible_models()` - Select final plausible models

### Visualization

- `plot_stability()` - Bar chart of stability scores
- `plot_aic_by_step()` - AIC distribution by model size
- `plot_model_tree()` - Tree diagram of model exploration
- `plot_variable_heatmap()` - Variable co-occurrence patterns
- `plot_model_dashboard()` - Comprehensive 4-panel overview
- `plot_stability_distribution()` - Bootstrap distributions
- `plot_variable_importance()` - Composite importance ranking

### Utilities

- `extract_all_models()` - Extract all models as data frame
- `confusion_metrics()` - Performance metrics for classification
- `variable_importance_ranking()` - Composite variable rankings

## Documentation

See the vignettes for detailed examples:
```r
# Basic usage with diabetes dataset
vignette("diabetes-example", package = "multipathaic")

# Publication-quality analysis
vignette("publication-quality", package = "multipathaic")
```

## Features

- Supports both **linear** and **logistic** regression
- Explores multiple competitive model paths simultaneously
- Quantifies variable stability through resampling
- Provides interpretable variable importance scores
- Professional publication-quality visualizations

## Citation

If you use this package, please cite:
```
Your Name et al. (2025). multipathaic: Multi-Path Model Selection with Stability Analysis.
R package version 1.0.0. https://github.com/R-4-Data-Science/multipathaic
```

## License

MIT License

## Authors

- Michael (Asante) Ofosu (Auburn University)
- Mohammad Al Srayheen  (Auburn University)
- Soroosh Alavi (Auburn University)
