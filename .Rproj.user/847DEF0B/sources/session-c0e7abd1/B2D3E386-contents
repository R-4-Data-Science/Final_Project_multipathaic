###############################################################################
# multipathaic
#
# Multi-Path Forward Selection with AIC and Stability Analysis
###############################################################################


###############################################################################
# OVERVIEW
###############################################################################
#
# `multipathaic` implements a novel and robust approach to model selection by
# exploring multiple promising paths through the model space, rather than
# following a single greedy forward-selection sequence.
#
# The package provides:
#
# - Multi-path forward selection driven by AIC
# - Bootstrap-based stability analysis
# - Plausible model identification through combined AIC and stability thresholds
# - Advanced publication-quality visualizations (model trees, heatmaps,
#   dashboards, importance rankings)
#
# This framework enhances transparency, robustness, and interpretability in
# variable selection.
#
###############################################################################


###############################################################################
# INSTALLATION
###############################################################################
#
# Install directly from GitHub:
#
# install.packages("remotes")    # Install remotes if needed
# remotes::install_github("R-4-Data-Science/Final_Project_multipathaic")
#
# Load the package:
#
# library(multipathaic)
#
###############################################################################


###############################################################################
# QUICK START (COPY AND RUN THIS BLOCK)
###############################################################################

library(multipathaic)

# Simulate example data
set.seed(123)
n <- 100
p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("x", 1:p)
y <- X[,1] + 0.5 * X[,2] + rnorm(n)

# Step 1: Multi-path forward selection
paths <- build_paths(X, y, family = "gaussian", K = 5, delta = 1.5)

# Step 2: Bootstrap stability analysis
stab <- stability(X, y, family = "gaussian", B = 50)

# Step 3: Identify plausible models
plausible <- plausible_models(paths, stab, Delta = 2, tau = 0.6)

# Step 4: Visualize exploration results
plot_model_tree(paths)
plot_stability(stab)
plot_variable_heatmap(plausible)

###############################################################################


###############################################################################
# RUNNING THE SHINY WEB APPLICATION
###############################################################################
#
# `multipathaic` includes a full interactive Shiny dashboard
# that allows users to run all analyses through a web browser.
#
# Launch the app:
#
# multipathaic::run_multipathaic_app()
#
# Features of the Shiny App:
#
# * Upload datasets (CSV)
# * Multi-path forward selection
# * Bootstrap stability analysis
# * Plausible model identification
# * Model tree visualization
# * Stability and importance plots
# * Downloadable summary tables and results
#
# The application opens automatically in your default web browser.
#
###############################################################################


###############################################################################
# MAIN FUNCTIONS
###############################################################################
#
# Core Algorithms
# ---------------
#
# * build_paths()        — Multi-path forward selection
# * stability()          — Bootstrap stability analysis
# * plausible_models()   — Select final plausible models
#
#
# Visualization Tools
# -------------------
#
# * plot_stability()              — Stability bar chart
# * plot_aic_by_step()            — AIC distribution across steps
# * plot_model_tree()             — Visual model-exploration tree
# * plot_variable_heatmap()       — Co-occurrence heatmap
# * plot_model_dashboard()        — Four-panel summary dashboard
# * plot_stability_distribution() — Bootstrap distribution plots
# * plot_variable_importance()    — Variable importance rankings
#
#
# Utility Functions
# -----------------
#
# * extract_all_models()          — Extract full model list
# * confusion_metrics()           — Binary classification metrics
# * variable_importance_ranking() — Combined importance score
#
###############################################################################


###############################################################################
# DOCUMENTATION
###############################################################################
#
# Access vignettes:
#
# vignette("diabetes-example", package = "multipathaic")
# vignette("publication-quality", package = "multipathaic")
#
###############################################################################


###############################################################################
# FEATURES
###############################################################################
#
# * Supports **linear** and **logistic** regression
# * Explores **multiple competitive model paths**
# * Quantifies **stability** via bootstrapping
# * Produces highly interpretable **importance scores**
# * Provides publication-quality visualizations for academic papers
#
###############################################################################


###############################################################################
# CITATION
###############################################################################
#
# If you use this package, please cite:
#
# multipathaic: Multi-Path AIC Model Selection with Stability Analysis
# R package version 1.0.0
# https://github.com/R-4-Data-Science/Final_Project_multipathaic.git
#
###############################################################################


###############################################################################
# LICENSE
###############################################################################
#
# MIT License.
#
###############################################################################


###############################################################################
# AUTHORS
###############################################################################
#
# * Michael Asante Ofosu
# * Mohammad Al Srayheen
# * Soroosh Alavi
#
###############################################################################
