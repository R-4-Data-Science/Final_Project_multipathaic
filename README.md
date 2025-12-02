
# Multipathaic  
### *A Multi-Path AIC Model Selection Framework with Stability Analysis*

`multipathaic` provides a transparent and robust alternative to traditional stepwise selection by exploring **multiple competitive forward-selection paths**, evaluating them with **AIC**, and validating their reliability using **bootstrap stability analysis**.  
It is designed for researchers who want **reproducible**, **interpretable**, and **publication-quality** model selection workflows.



## 🚀 Key Features

- **Multi-path forward selection** driven by AIC  
- **Bootstrap-based stability analysis**  
- Automated **plausible model** identification using AIC and stability thresholds  
- Publication-quality plots:  
  - Model tree  
  - Variable co-occurrence heatmap  
  - Stability bar charts  
  - Importance rankings  
  - Four-panel model dashboard  
- Fully integrated **Shiny Web Application**  
- Supports both **linear** and **logistic** regression  



## 📦 Installation

```r
install.packages("remotes")
remotes::install_github("R-4-Data-Science/Final_Project_multipathaic")

library(multipathaic)
````



## 🧪 Quick Start Example

```r
library(multipathaic)

# Simulated example
set.seed(123)
n <- 100
p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("x", 1:p)
y <- X[,1] + 0.5 * X[,2] + rnorm(n)

# 1. Build multi-path model-selection tree
paths <- build_paths(
  X, y,
  family = "gaussian",
  K = 5,          # number of forward paths
  delta = 1.5     # AIC competitiveness threshold
)

# 2. Bootstrap stability
stab <- stability(
  X, y,
  family = "gaussian",
  B = 50          # number of bootstrap samples
)

# 3. Identify plausible models
plausible <- plausible_models(
  paths, stab,
  Delta = 2,      # AIC tolerance
  tau = 0.6       # stability threshold
)

# 4. Visualizations
plot_model_tree(paths)
plot_stability(stab)
plot_variable_heatmap(plausible)
```



## 🌐 Shiny Web Application

`multipathaic` includes a complete browser-based dashboard for interactive analysis.

### ▶️ Launch the app

```r
multipathaic::run_multipathaic_app()
```

### App Capabilities

* Upload CSV datasets
* Explore variables (histograms, scatterplots, correlations)
* Run multi-path AIC selection
* Perform bootstrap stability
* Identify plausible models
* Visualize model tree, heatmaps, importance, and dashboards
* Download tables and plots
* Generate automated reports



## 🧠 Main Functions

### Core Algorithms

| Function             | Purpose                      |
| -------------------- | ---------------------------- |
| `build_paths()`      | Multi-path forward selection |
| `stability()`        | Bootstrap stability analysis |
| `plausible_models()` | Final model identification   |

### Visualization Tools

| Plot                         | Description                    |
| ---------------------------- | ------------------------------ |
| `plot_stability()`           | Stability bar chart            |
| `plot_model_tree()`          | Visual model-exploration tree  |
| `plot_variable_heatmap()`    | Variable co-occurrence heatmap |
| `plot_aic_by_step()`         | AIC improvements per step      |
| `plot_model_dashboard()`     | Four-panel summary dashboard   |
| `plot_variable_importance()` | Combined importance ranking    |

### Utilities

* `extract_all_models()`
* `confusion_metrics()`
* `variable_importance_ranking()`



## 📘 Documentation & Vignettes

```r
vignette("diabetes-example", package = "multipathaic")
vignette("publication-quality", package = "multipathaic")
```



## 📚 Citation

If you use this package, please cite:

**multipathaic: Multi-Path AIC Model Selection with Stability Analysis**
R package version 1.0.0
[https://github.com/R-4-Data-Science/Final_Project_multipathaic](https://github.com/R-4-Data-Science/Final_Project_multipathaic)



## 📄 License

MIT License.



## 👥 Authors

* **Michael Asante Ofosu**
* **Mohammad Al Srayheen**
* **Soroosh Alavi**

