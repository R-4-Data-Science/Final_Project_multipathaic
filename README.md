
*A Multi-Path AIC Model Selection Framework with Stability Analysis*

`multipathaic` provides a robust alternative to traditional stepwise selection by exploring **multiple forward-selection paths** simultaneously and combining **AIC** with **bootstrap stability**. The package is designed for researchers who want transparent, reproducible, and interpretable model-selection workflows.


## 🔹 Key Features

- Multi-path forward selection driven by **AIC**
- **Bootstrap-based stability analysis**
- Identification of **plausible models** using AIC and stability thresholds
- Publication-quality visualizations:
  - Model tree
  - Variable co-occurrence heatmap
  - Stability plots
  - Importance rankings
- Fully integrated **Shiny dashboard** for interactive exploration
- Supports **linear** and **logistic** regression



## 🔹 Installation

```r
install.packages("remotes")  
remotes::install_github("R-4-Data-Science/Final_Project_multipathaic")

library(multipathaic)
````


## 🔹 Quick Start Example

Here is a minimal example you can copy and run:

```r
library(multipathaic)

# Simulated example data
set.seed(123)
n <- 100
p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("x", 1:p)
y <- X[,1] + 0.5 * X[,2] + rnorm(n)

# 1. Build multi-path selection tree
paths <- build_paths(
  X, y,
  family = "gaussian",
  K = 5,         # number of paths
  delta = 1.5    # AIC competitiveness threshold
)

# 2. Bootstrap stability analysis
stab <- stability(
  X, y,
  family = "gaussian",
  B = 50         # number of bootstrap samples
)

# 3. Identify plausible models
plausible <- plausible_models(
  paths, stab,
  Delta = 2,     # AIC tolerance
  tau = 0.6      # stability threshold
)

# 4. Visualizations
plot_model_tree(paths)
plot_stability(stab)
plot_variable_heatmap(plausible)
```


## 🔹 Shiny Web Application

`multipathaic` includes a complete browser-based dashboard.

Launch it with:

```r
multipathaic::run_multipathaic_app()
```

### App Features

* CSV dataset upload
* Multi-path AIC selection
* Bootstrap stability
* Plausible model identification
* Model tree visualization
* Heatmaps, variable importance, and dashboards
* Downloadable summaries and results



## 🔹 Main Functions

### Core Algorithms

| Function             | Description                    |
| -------------------- | ------------------------------ |
| `build_paths()`      | Multi-path forward selection   |
| `stability()`        | Bootstrap stability analysis   |
| `plausible_models()` | Selects final plausible models |

### Visualization Tools

| Plot                         | Purpose                |
| ---------------------------- | ---------------------- |
| `plot_stability()`           | Stability bar chart    |
| `plot_model_tree()`          | Model-exploration tree |
| `plot_variable_heatmap()`    | Variable co-occurrence |
| `plot_aic_by_step()`         | AIC evolution          |
| `plot_model_dashboard()`     | Four-panel summary     |
| `plot_variable_importance()` | Importance ranking     |

### Utilities

* `extract_all_models()`
* `confusion_metrics()`
* `variable_importance_ranking()`



## 🔹 Documentation

Use the built-in vignettes:

```r
vignette("diabetes-example", package = "multipathaic")
vignette("publication-quality", package = "multipathaic")
```



## 🔹 Citation

If you use this package, please cite:

**multipathaic: Multi-Path AIC Model Selection with Stability Analysis**
R package version 1.0.0
[https://github.com/R-4-Data-Science/Final_Project_multipathaic](https://github.com/R-4-Data-Science/Final_Project_multipathaic)



## 🔹 License

MIT License.



## 🔹 Authors

* **Michael Asante Ofosu**
* **Mohammad Al Srayheen**
* **Soroosh Alavi**


