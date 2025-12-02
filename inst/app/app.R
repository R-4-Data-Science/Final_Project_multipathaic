# ==============================================================================
# MODERNIZED MULTIPATHAIC PROFESSIONAL SHINY APPLICATION
# Version 3.0.0 - Premium Cream/Brown Design
# FULLY RESPONSIVE | FIXED ICONS | MODERN UI/UX
# ==============================================================================
# IMPORTANT: Save this file OUTSIDE your package directory!
# Run with: source("multipathaic_professional_modernized.R") or click "Run App"
# ==============================================================================
#install.packages("shinydashboard")
install.packages("multipathaic")


# Load required libraries
library(shiny)
library(shinydashboard)
library(multipathaic)
library(DT)
library(plotly)
library(shinyWidgets)
library(colourpicker)
library(ggplot2)
library(shinyjs)
library(gridExtra)
library(scales)

# ==============================================================================
# ULTRA-LIGHT PROFESSIONAL COLOR PALETTE (CLOSE TO WHITE)
# ==============================================================================
app_colors <- list(
  primary = "#C5B5A0",        # very light taupe
  secondary = "#E8DFD4",      # almost white beige
  accent = "#D4C4B0",         # light warm gray
  background = "#FCFCFC",     # almost pure white
  card = "#FFFFFF",           # pure white cards
  text_dark = "#2D2D2D",      # dark gray text (readable)
  text_light = "#666666",     # medium gray text
  hover = "#F7F5F2",          # subtle hover (barely visible)
  border = "#E8E8E8",         # very light gray border
  success = "#8FBC8F",        # soft green
  warning = "#DEB887",        # soft orange/tan
  danger = "#CD5C5C",         # soft red
  info = "#87CEEB",           # sky blue
  shadow = "rgba(0, 0, 0, 0.04)",      # barely visible shadow
  shadow_hover = "rgba(0, 0, 0, 0.08)"  # subtle hover shadow
)

# ==============================================================================
# USER INTERFACE
# ==============================================================================

ui <- dashboardPage(
  skin = "black",

  # ============================================================================
  # HEADER
  # ============================================================================
  dashboardHeader(
    title = span(
      tags$i(class = "fas fa-chart-line", style = "margin-right: 12px;"),
      "multipathaic Professional"
    ),
    titleWidth = 450,
    tags$li(
      class = "dropdown",
      style = "padding: 12px 15px; color: white; font-weight: 500;",
      tags$i(class = "far fa-clock", style = "margin-right: 8px;"),
      textOutput("current_time_display", inline = TRUE)
    ),
    tags$li(
      class = "dropdown",
      actionButton(
        "help_btn",
        HTML('<i class="fas fa-question-circle" style="margin-right: 6px;"></i> Help'),
        style = sprintf(
          "margin-top: 8px; background: %s; border: none; color: white;
          border-radius: 20px; padding: 8px 20px; font-weight: 600;
          transition: all 0.3s ease; box-shadow: 0 2px 8px rgba(122,167,214,0.3);",
          app_colors$info
        )
      )
    )
  ),

  # ============================================================================
  # SIDEBAR
  # ============================================================================
  dashboardSidebar(
    width = 320,
    useShinyjs(),

    # Sidebar Header
    div(
      style = sprintf(
        "padding: 25px 20px; text-align: center;
        background: linear-gradient(135deg, %s 0%%, %s 100%%);
        border-bottom: 1px solid %s;",
        app_colors$secondary, app_colors$background, app_colors$border
      ),
      tags$h4(
        tags$i(class = "fas fa-chart-area", style = "margin-right: 10px;"),
        "Statistical Analytics",
        style = sprintf("color: %s; margin: 0; font-weight: 500; font-size: 16px;", app_colors$text_dark)
      )
    ),

    # Sidebar Menu
    sidebarMenu(
      id = "sidebar",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
      menuItem("Data Management", tabName = "data", icon = icon("database")),
      menuItem("Data Cleaning", tabName = "cleaning", icon = icon("broom")),
      menuItem("Exploration", tabName = "explore", icon = icon("search")),
      menuItem("Multi-Path", tabName = "paths", icon = icon("project-diagram")),
      menuItem("Stability", tabName = "stability", icon = icon("chart-line")),
      menuItem("Plausible Models", tabName = "plausible", icon = icon("star")),
      menuItem("Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("stethoscope")),
      menuItem("Plots", tabName = "plots", icon = icon("palette")),
      menuItem("Reports", tabName = "reports", icon = icon("file-alt")),
      menuItem("Downloads", tabName = "download", icon = icon("download")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),

    hr(style = sprintf("border-color: %s; margin: 20px 15px;", app_colors$border)),

    # Session Stats Box
    div(
      style = sprintf(
        "padding: 20px; background: %s;
        margin: 15px; border-radius: 16px;
        border: 1px solid %s;",
        app_colors$card, app_colors$border
      ),
      h5(
        tags$i(class = "fas fa-chart-bar", style = "margin-right: 8px;"),
        "Session Stats",
        style = sprintf("color: %s; margin-bottom: 15px; font-size: 14px; font-weight: 600;", app_colors$text_dark)
      ),
      uiOutput("sidebar_stats")
    )
  ),

  # ============================================================================
  # BODY
  # ============================================================================
  dashboardBody(
    useShinyjs(),

    # Custom CSS and Font Awesome
    tags$head(
      # Font Awesome 6
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
      ),
      # Google Fonts
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&family=Nunito:wght@300;400;600;700;800&display=swap"
      ),

      tags$style(HTML(paste0("
        /* ============================================================
           GLOBAL STYLES
        ============================================================ */
        * {
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif !important;
          -webkit-font-smoothing: antialiased;
          -moz-osx-font-smoothing: grayscale;
        }

        body, .content-wrapper {
          background: linear-gradient(135deg, ", app_colors$background, " 0%, #FAF7F2 50%, ", app_colors$background, " 100%) !important;
          min-height: 100vh;
        }

        .content-wrapper {
          background-attachment: fixed !important;
        }

        /* ============================================================
           SIDEBAR STYLING
        ============================================================ */
        .main-sidebar {
          background: linear-gradient(180deg, ", app_colors$secondary, " 0%, ", app_colors$background, " 50%, ", app_colors$secondary, " 100%) !important;
          box-shadow: 2px 0 12px rgba(0,0,0,0.04);
          border-right: 1px solid ", app_colors$border, ";
        }

        /* Force Font Awesome icons to display */
        .sidebar-menu i, .fa, .fas, .far, .fal, .fab {
          font-family: 'Font Awesome 6 Free' !important;
          font-weight: 900;
          font-style: normal;
          font-variant: normal;
          text-rendering: auto;
          -webkit-font-smoothing: antialiased;
        }

        .far {
          font-weight: 400 !important;
        }

        .sidebar-menu > li > a {
          color: ", app_colors$text_dark, " !important;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          padding: 16px 20px !important;
          border-left: 4px solid transparent !important;
          font-weight: 500;
          font-size: 14px;
        }

        .sidebar-menu > li > a:hover {
          background: ", app_colors$hover, " !important;
          border-left-color: ", app_colors$primary, " !important;
          transform: translateX(4px);
          color: ", app_colors$text_dark, " !important;
        }

        .sidebar-menu > li.active > a {
          border-left-color: ", app_colors$primary, " !important;
          background: ", app_colors$hover, " !important;
          color: ", app_colors$text_dark, " !important;
          font-weight: 600;
          box-shadow: inset 0 0 20px rgba(0,0,0,0.03);
        }

        .sidebar-menu > li > a > i {
          margin-right: 12px;
          font-size: 16px;
          width: 20px;
          text-align: center;
          transition: all 0.3s ease;
          color: ", app_colors$text_light, " !important;
        }

        .sidebar-menu > li > a:hover > i,
        .sidebar-menu > li.active > a > i {
          transform: scale(1.15);
          color: ", app_colors$primary, " !important;
        }

        /* ============================================================
           HEADER STYLING
        ============================================================ */
        .main-header .navbar {
          background: linear-gradient(135deg, ", app_colors$background, " 0%, ", app_colors$secondary, " 100%) !important;
          border: none !important;
          box-shadow: 0 2px 8px rgba(0,0,0,0.06);
          border-bottom: 1px solid ", app_colors$border, ";
        }

        .main-header .logo {
          background: ", app_colors$secondary, " !important;
          color: ", app_colors$text_dark, " !important;
          font-weight: 700 !important;
          font-size: 16px !important;
          letter-spacing: 0.3px;
          border-right: 1px solid ", app_colors$border, ";
        }

        .main-header .logo:hover {
          background: ", app_colors$primary, " !important;
        }

        /* ============================================================
           BOX STYLING - MODERN CARDS
        ============================================================ */
        .box {
          border-radius: 20px !important;
          box-shadow: 0 4px 20px ", app_colors$shadow, " !important;
          border: 1px solid ", app_colors$border, " !important;
          background: ", app_colors$card, " !important;
          transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
          overflow: hidden;
          margin-bottom: 25px;
        }

        .box:hover {
          transform: translateY(-8px);
          box-shadow: 0 12px 32px ", app_colors$shadow_hover, " !important;
          border-color: ", app_colors$secondary, " !important;
        }

        .box-header {
          background: linear-gradient(135deg, ", app_colors$secondary, " 0%, ", app_colors$background, " 100%) !important;
          color: ", app_colors$text_dark, " !important;
          border-radius: 20px 20px 0 0 !important;
          padding: 20px 25px !important;
          border-bottom: 1px solid ", app_colors$border, " !important;
        }

        .box-title {
          font-weight: 700 !important;
          font-size: 17px !important;
          letter-spacing: 0.3px;
          display: flex;
          align-items: center;
        }

        .box-title i {
          margin-right: 10px;
          font-size: 18px;
        }

        .box-body {
          padding: 25px !important;
        }

        /* ============================================================
           VALUE BOXES - DASHBOARD METRICS
        ============================================================ */
        .small-box {
          border-radius: 18px !important;
          box-shadow: 0 4px 16px ", app_colors$shadow, " !important;
          transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
          border: 1px solid rgba(255,255,255,0.6);
          overflow: hidden;
          position: relative;
        }

        .small-box::before {
          content: '';
          position: absolute;
          top: -50%%;
          right: -50%%;
          width: 200%%;
          height: 200%%;
          background: radial-gradient(circle, rgba(255,255,255,0.1) 0%%, transparent 70%%);
          transition: all 0.6s ease;
        }

        .small-box:hover {
          transform: translateY(-10px) scale(1.02);
          box-shadow: 0 12px 32px ", app_colors$shadow_hover, " !important;
        }

        .small-box:hover::before {
          top: -25%%;
          right: -25%%;
        }

        .small-box h3 {
          font-size: 38px !important;
          font-weight: 800 !important;
          margin: 0 0 10px 0 !important;
        }

        .small-box p {
          font-size: 15px !important;
          font-weight: 600 !important;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }

        .small-box-footer {
          padding: 12px 0 !important;
          font-weight: 600 !important;
          font-size: 13px !important;
        }

        .small-box .icon {
          font-size: 90px !important;
          top: 15px !important;
          right: 20px !important;
          opacity: 0.15 !important;
          font-family: 'Font Awesome 6 Free' !important;
          font-weight: 900 !important;
        }

        .small-box.bg-aqua { background-color: ", app_colors$info, " !important; }
        .small-box.bg-green { background-color: ", app_colors$success, " !important; }
        .small-box.bg-yellow { background-color: ", app_colors$warning, " !important; }
        .small-box.bg-red { background-color: ", app_colors$danger, " !important; }

        /* ============================================================
           BUTTONS - MODERN STYLING
        ============================================================ */
        .btn {
          border-radius: 30px !important;
          padding: 14px 36px !important;
          font-weight: 700 !important;
          font-size: 14px !important;
          text-transform: uppercase;
          letter-spacing: 0.8px;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          border: none !important;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          position: relative;
          overflow: hidden;
        }

        .btn::before {
          content: '';
          position: absolute;
          top: 50%%;
          left: 50%%;
          width: 0;
          height: 0;
          border-radius: 50%%;
          background: rgba(255,255,255,0.3);
          transform: translate(-50%%, -50%%);
          transition: width 0.6s, height 0.6s;
        }

        .btn:hover::before {
          width: 300px;
          height: 300px;
        }

        .btn:hover {
          transform: translateY(-3px);
          box-shadow: 0 8px 20px rgba(0,0,0,0.25);
        }

        .btn:active {
          transform: translateY(-1px);
        }

        .btn-primary {
          background: linear-gradient(135deg, ", app_colors$primary, " 0%, ", app_colors$accent, " 100%) !important;
          color: white !important;
        }

        .btn-success {
          background: linear-gradient(135deg, ", app_colors$success, " 0%, #96BA7D 100%%) !important;
          color: white !important;
        }

        .btn-warning {
          background: linear-gradient(135deg, ", app_colors$warning, " 0%, #E8BE87 100%%) !important;
          color: white !important;
        }

        .btn-danger {
          background: linear-gradient(135deg, ", app_colors$danger, " 0%, #D87F78 100%%) !important;
          color: white !important;
        }

        .btn-info {
          background: linear-gradient(135deg, ", app_colors$info, " 0%, #95BCE4 100%%) !important;
          color: white !important;
        }

        .btn-block {
          width: 100%% !important;
        }

        /* ============================================================
           FORM CONTROLS
        ============================================================ */
        .form-control, .selectize-input {
          border: 2px solid ", app_colors$border, " !important;
          border-radius: 12px !important;
          padding: 12px 18px !important;
          transition: all 0.3s ease;
          background: white !important;
          font-size: 14px !important;
        }

        .form-control:focus, .selectize-input.focus {
          border-color: ", app_colors$primary, " !important;
          box-shadow: 0 0 0 4px rgba(166,139,113,0.12) !important;
          transform: translateY(-1px);
        }

        .form-group label {
          font-weight: 600 !important;
          color: ", app_colors$text_dark, " !important;
          font-size: 13px !important;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          margin-bottom: 8px !important;
        }

        /* ============================================================
           TABLES - DATA TABLES
        ============================================================ */
        table.dataTable {
          border-radius: 12px !important;
          overflow: hidden;
          border: 1px solid ", app_colors$border, " !important;
        }

        table.dataTable thead {
          background: linear-gradient(135deg, ", app_colors$primary, " 0%, ", app_colors$accent, " 100%) !important;
          color: white !important;
        }

        table.dataTable thead th {
          padding: 18px 15px !important;
          font-weight: 700 !important;
          font-size: 13px !important;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          border: none !important;
        }

        table.dataTable tbody tr {
          transition: all 0.3s ease;
        }

        table.dataTable tbody tr:hover {
          background: ", app_colors$hover, " !important;
          transform: scale(1.01);
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        }

        table.dataTable tbody td {
          padding: 14px 15px !important;
          font-size: 13px !important;
          border-bottom: 1px solid ", app_colors$border, " !important;
        }

        /* ============================================================
           TYPOGRAPHY
        ============================================================ */
        h1, h2, h3, h4, h5, h6 {
          color: ", app_colors$text_dark, " !important;
          font-weight: 700 !important;
          letter-spacing: 0.3px;
        }

        h1 { font-size: 32px !important; }
        h2 { font-size: 28px !important; }
        h3 { font-size: 24px !important; }
        h4 { font-size: 20px !important; }
        h5 { font-size: 16px !important; }

        p {
          color: ", app_colors$text_dark, " !important;
          line-height: 1.7;
          font-size: 14px;
        }

        /* ============================================================
           ALERTS
        ============================================================ */
        .alert {
          border-radius: 16px !important;
          padding: 18px 22px !important;
          border: none !important;
          font-weight: 600 !important;
          box-shadow: 0 4px 12px rgba(0,0,0,0.08);
          display: flex;
          align-items: center;
        }

        .alert i {
          margin-right: 12px;
          font-size: 20px;
        }

        .alert-info {
          background: linear-gradient(135deg, #E3F2FD 0%%, #BBDEFB 100%%) !important;
          color: #1565C0 !important;
        }

        .alert-success {
          background: linear-gradient(135deg, #E8F5E9 0%%, #C8E6C9 100%%) !important;
          color: #2E7D32 !important;
        }

        .alert-warning {
          background: linear-gradient(135deg, #FFF3E0 0%%, #FFE0B2 100%%) !important;
          color: #E65100 !important;
        }

        .alert-danger {
          background: linear-gradient(135deg, #FFEBEE 0%%, #FFCDD2 100%%) !important;
          color: #C62828 !important;
        }

        /* ============================================================
           LOADING SPINNER
        ============================================================ */
        .shiny-output-error { display: none; }

        .shiny-spinner-loading {
          border: 4px solid ", app_colors$border, ";
          border-top: 4px solid ", app_colors$primary, ";
          border-radius: 50%%;
          width: 50px;
          height: 50px;
          animation: spin 1s linear infinite;
          margin: 40px auto;
        }

        @keyframes spin {
          0%% { transform: rotate(0deg); }
          100%% { transform: rotate(360deg); }
        }

        /* ============================================================
           HR DIVIDERS
        ============================================================ */
        hr {
          border: none;
          height: 2px;
          background: linear-gradient(to right, transparent, ", app_colors$border, ", transparent);
          margin: 25px 0;
        }

        /* ============================================================
           SCROLLBAR
        ============================================================ */
        ::-webkit-scrollbar {
          width: 10px;
          height: 10px;
        }

        ::-webkit-scrollbar-track {
          background: ", app_colors$hover, ";
          border-radius: 10px;
        }

        ::-webkit-scrollbar-thumb {
          background: ", app_colors$secondary, ";
          border-radius: 10px;
          transition: all 0.3s ease;
        }

        ::-webkit-scrollbar-thumb:hover {
          background: ", app_colors$primary, ";
        }

        /* ============================================================
           RESPONSIVE DESIGN
        ============================================================ */
        @media (max-width: 768px) {
          .box {
            margin-bottom: 20px;
          }

          .btn {
            padding: 12px 24px !important;
            font-size: 12px !important;
          }

          .small-box h3 {
            font-size: 28px !important;
          }

          .content-wrapper {
            padding: 15px !important;
          }
        }

        /* ============================================================
           MODAL STYLING
        ============================================================ */
        .modal-content {
          border-radius: 20px !important;
          border: none !important;
          box-shadow: 0 20px 60px rgba(0,0,0,0.3) !important;
        }

        .modal-header {
          background: linear-gradient(135deg, ", app_colors$primary, " 0%, ", app_colors$accent, " 100%) !important;
          color: white !important;
          border-radius: 20px 20px 0 0 !important;
          padding: 20px 30px !important;
          border-bottom: none !important;
        }

        .modal-title {
          font-weight: 700 !important;
          font-size: 20px !important;
        }

        .modal-body {
          padding: 30px !important;
        }

        .modal-footer {
          border-top: 1px solid ", app_colors$border, " !important;
          padding: 20px 30px !important;
        }

        /* ============================================================
           PLOTLY STYLING
        ============================================================ */
        .plotly {
          border-radius: 12px;
          overflow: hidden;
        }

        /* ============================================================
           SHINY NOTIFICATION
        ============================================================ */
        .shiny-notification {
          border-radius: 16px !important;
          box-shadow: 0 8px 24px rgba(0,0,0,0.2) !important;
          border-left: 5px solid ", app_colors$primary, " !important;
          font-weight: 600 !important;
        }

        /* ============================================================
           SPECIAL EFFECTS
        ============================================================ */
        .premium-card {
          position: relative;
          overflow: hidden;
        }

        .premium-card::before {
          content: '';
          position: absolute;
          top: 0;
          left: -100%%;
          width: 100%%;
          height: 100%%;
          background: linear-gradient(90deg, transparent, rgba(255,255,255,0.3), transparent);
          transition: left 0.7s;
        }

        .premium-card:hover::before {
          left: 100%%;
        }
      ")))
    ),

    # ============================================================================
    # TAB ITEMS
    # ============================================================================
    tabItems(

      # ==========================================================================
      # DASHBOARD TAB
      # ==========================================================================
      tabItem(
        tabName = "dashboard",

        # Welcome Card
        fluidRow(
          box(
            width = 12,
            solidHeader = FALSE,
            class = "premium-card",
            div(
              style = sprintf(
                "background: white; border-radius: 20px; padding: 40px;
                box-shadow: 0 4px 20px %s; border: 1px solid %s;",
                app_colors$shadow, app_colors$border
              ),
              div(
                style = sprintf(
                  "background: linear-gradient(135deg, %s 0%%, %s 100%%);
                  color: white; padding: 20px 30px; border-radius: 16px;
                  margin: -40px -40px 30px -40px;",
                  app_colors$primary, app_colors$accent
                ),
                tags$h3(
                  tags$i(class = "fas fa-home", style = "margin-right: 12px;"),
                  "Welcome to Multipathaic Professional Suite",
                  style = "margin: 0; font-weight: 700; font-size: 24px;"
                )
              ),

              fluidRow(
                column(
                  8,
                  h3("Advanced Multi-Path Variable Selection", style = "margin-top: 0; font-size: 22px;"),
                  p(
                    "State-of-the-art statistical tool for robust model selection using
                    AIC-based multi-path forward selection combined with stability analysis.",
                    style = "font-size: 15px; line-height: 1.8;"
                  ),
                  hr(),
                  h4(
                    tags$i(class = "fas fa-rocket", style = "margin-right: 10px;"),
                    "Quick Start Guide"
                  ),
                  tags$ol(
                    style = "line-height: 2.2; font-size: 14px;",
                    tags$li(tags$b("Load your data"), " or use built-in datasets"),
                    tags$li(tags$b("Clean and explore"), " your data thoroughly"),
                    tags$li(tags$b("Run Multi-Path Analysis"), " with optimal parameters"),
                    tags$li(tags$b("Run Stability Analysis"), " via bootstrap/subsample"),
                    tags$li(tags$b("Select plausible models"), " using AIC and stability"),
                    tags$li(tags$b("Generate comprehensive reports"), " and visualizations")
                  ),
                  hr(),
                  actionButton(
                    "start_analysis",
                    HTML('<i class="fas fa-play-circle"></i> Start Analysis'),
                    class = "btn-primary",
                    style = "margin-right: 15px; margin-bottom: 10px;"
                  ),
                  actionButton(
                    "load_example",
                    HTML('<i class="fas fa-folder-open"></i> Load mtcars Example'),
                    class = "btn-info",
                    style = "margin-bottom: 10px;"
                  )
                ),
                column(
                  4,
                  div(
                    style = sprintf(
                      "text-align: center; padding: 40px 30px;
                      background: linear-gradient(135deg, %s 0%%, %s 100%%);
                      border-radius: 20px; box-shadow: 0 8px 24px rgba(0,0,0,0.1);",
                      app_colors$hover, app_colors$secondary
                    ),
                    tags$i(
                      class = "fas fa-chart-line",
                      style = sprintf("font-size: 80px; color: %s; margin-bottom: 20px;", app_colors$primary)
                    ),
                    h4("Ready to Analyze", style = sprintf("color: %s; margin: 0; font-weight: 700;", app_colors$text_dark))
                  )
                )
              )
            )
          )
        ),

        # Value Boxes
        fluidRow(
          valueBoxOutput("dash_observations", width = 3),
          valueBoxOutput("dash_predictors", width = 3),
          valueBoxOutput("dash_models_found", width = 3),
          valueBoxOutput("dash_status", width = 3)
        ),

        # Activity and Recommendations
        fluidRow(
          box(
            title = HTML('<i class="fas fa-list-ul"></i> Activity Log'),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "450px",
            div(
              style = "max-height: 360px; overflow-y: auto; padding-right: 10px;",
              uiOutput("activity_log_display")
            )
          ),
          box(
            title = HTML('<i class="fas fa-lightbulb"></i> Smart Recommendations'),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            height = "450px",
            uiOutput("smart_recommendations_display")
          )
        ),

        # Quick Actions
        fluidRow(
          box(
            title = HTML('<i class="fas fa-bolt"></i> Quick Actions'),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(2, actionButton("quick_load", HTML('<i class="fas fa-database"></i> Load Data'), class = "btn-primary btn-block")),
              column(2, actionButton("quick_clean", HTML('<i class="fas fa-broom"></i> Clean'), class = "btn-success btn-block")),
              column(2, actionButton("quick_paths", HTML('<i class="fas fa-project-diagram"></i> Paths'), class = "btn-warning btn-block")),
              column(2, actionButton("quick_stability", HTML('<i class="fas fa-chart-line"></i> Stability'), class = "btn-info btn-block")),
              column(2, actionButton("quick_plausible", HTML('<i class="fas fa-star"></i> Plausible'), class = "btn-danger btn-block")),
              column(2, actionButton("quick_report", HTML('<i class="fas fa-file-alt"></i> Report'), class = "btn-primary btn-block"))
            )
          )
        )
      ),

      # ==========================================================================
      # DATA MANAGEMENT TAB
      # ==========================================================================
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-folder-open"></i> Data Source'),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            radioButtons(
              "data_source",
              "Choose source:",
              choices = c("Built-in Dataset" = "builtin", "Upload CSV" = "upload")
            ),
            conditionalPanel(
              condition = "input.data_source == 'builtin'",
              selectInput(
                "builtin_dataset",
                "Select Dataset:",
                choices = c(
                  "mtcars (Motor Trend)" = "mtcars",
                  "iris (Fisher's Iris)" = "iris",
                  "Boston Housing (MASS)" = "boston",
                  "Diabetes (MASS)" = "diabetes"
                )
              )
            ),
            conditionalPanel(
              condition = "input.data_source == 'upload'",
              fileInput(
                "file_upload",
                "Upload CSV File:",
                accept = c(".csv", "text/csv"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
              ),
              fluidRow(
                column(6, selectInput("sep", "Separator:", choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"))),
                column(6, selectInput("dec", "Decimal:", choices = c("Period" = ".", "Comma" = ",")))
              ),
              checkboxInput("header", "File has header row", TRUE)
            ),
            hr(),
            actionButton(
              "preview_data",
              HTML('<i class="fas fa-eye"></i> Preview Data'),
              class = "btn-warning btn-block"
            )
          ),

          box(
            title = HTML('<i class="fas fa-bullseye"></i> Model Configuration'),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            selectInput("response_var", "Response Variable (y):", choices = NULL),
            selectInput("predictor_vars", "Predictor Variables (X):", choices = NULL, multiple = TRUE),
            selectInput(
              "family",
              "Model Family:",
              choices = c(
                "Linear Regression (Gaussian)" = "gaussian",
                "Logistic Regression (Binomial)" = "binomial"
              )
            ),
            hr(),
            h5(tags$i(class = "fas fa-cogs", style = "margin-right: 8px;"), "Preprocessing Options:"),
            checkboxInput("remove_na", "Remove rows with missing values", TRUE),
            checkboxInput("scale_vars", "Standardize predictors (z-score)", FALSE),
            checkboxInput("handle_categorical", "Auto-convert categorical variables", TRUE),
            hr(),
            actionButton(
              "load_data",
              HTML('<i class="fas fa-check-circle"></i> Load & Prepare Data'),
              class = "btn-success btn-block"
            )
          )
        ),

        # Data Summary Boxes
        fluidRow(
          valueBoxOutput("n_obs", width = 3),
          valueBoxOutput("n_pred", width = 3),
          valueBoxOutput("response_type", width = 3),
          valueBoxOutput("data_status", width = 3)
        ),

        # Data Preview
        fluidRow(
          box(
            title = HTML('<i class="fas fa-table"></i> Data Preview'),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("raw_data_preview")
          )
        ),

        # Data Quality Report
        fluidRow(
          box(
            title = HTML('<i class="fas fa-clipboard-check"></i> Data Quality Report'),
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("data_quality")
          )
        )
      ),

      # ==========================================================================
      # DATA CLEANING TAB
      # ==========================================================================
      tabItem(
        tabName = "cleaning",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-filter"></i> Data Filters'),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            selectInput("filter_var", "Select Variable:", choices = NULL),
            conditionalPanel(
              condition = "output.filter_var_type == 'numeric'",
              radioButtons(
                "filter_type_num",
                "Filter Type:",
                choices = c(
                  "Range Filter" = "range",
                  "Outlier Detection" = "outliers",
                  "Custom Threshold" = "custom"
                )
              ),
              conditionalPanel(
                condition = "input.filter_type_num == 'range'",
                uiOutput("filter_range_ui")
              ),
              conditionalPanel(
                condition = "input.filter_type_num == 'outliers'",
                sliderInput("outlier_sd", "Standard Deviation Threshold:", 1, 5, 3, 0.5)
              )
            ),
            hr(),
            actionButton("add_filter", HTML('<i class="fas fa-plus-circle"></i> Add Filter'), class = "btn-primary"),
            actionButton("clear_filters", HTML('<i class="fas fa-trash"></i> Clear All'), class = "btn-danger", style = "margin-left: 10px;"),
            hr(),
            h5(tags$i(class = "fas fa-list", style = "margin-right: 8px;"), "Active Filters:"),
            verbatimTextOutput("active_filters")
          ),

          box(
            title = HTML('<i class="fas fa-broom"></i> Cleaning Operations'),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            h5(tags$i(class = "fas fa-question-circle", style = "margin-right: 8px;"), "Missing Values:"),
            radioButtons(
              "missing_method",
              "Handling Method:",
              choices = c(
                "Remove rows with NA" = "remove",
                "Impute (mean/mode)" = "impute",
                "Keep as is" = "keep"
              )
            ),
            hr(),
            checkboxInput("remove_duplicates", "Remove duplicate rows", FALSE),
            hr(),
            h5(tags$i(class = "fas fa-exclamation-triangle", style = "margin-right: 8px;"), "Outlier Detection:"),
            checkboxInput("detect_outliers", "Enable outlier detection", TRUE),
            conditionalPanel(
              condition = "input.detect_outliers",
              selectInput(
                "outlier_method",
                "Detection Method:",
                choices = c(
                  "IQR Method" = "iqr",
                  "Z-Score Method" = "zscore",
                  "MAD Method" = "mad"
                )
              )
            ),
            hr(),
            actionButton(
              "apply_cleaning",
              HTML('<i class="fas fa-magic"></i> Apply Cleaning'),
              class = "btn-success btn-block"
            )
          )
        ),

        fluidRow(
          box(
            title = HTML('<i class="fas fa-check-double"></i> Cleaned Data Preview'),
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("cleaned_data_preview")
          )
        )
      ),

      # ==========================================================================
      # EXPLORATION TAB
      # ==========================================================================
      tabItem(
        tabName = "explore",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-chart-bar"></i> Distribution Analysis'),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            selectInput("explore_var", "Select Variable:", choices = NULL),
            plotlyOutput("explore_histogram", height = "400px")
          ),
          box(
            title = HTML('<i class="fas fa-project-diagram"></i> Correlation Matrix'),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("explore_correlation", height = "400px")
          )
        ),
        fluidRow(
          box(
            title = HTML('<i class="fas fa-scatter-chart"></i> Scatter Plot'),
            status = "success",
            solidHeader = TRUE,
            width = 6,
            selectInput("explore_predictor", "Select Predictor:", choices = NULL),
            plotlyOutput("explore_scatter", height = "400px")
          ),
          box(
            title = HTML('<i class="fas fa-table"></i> Summary Statistics'),
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            DTOutput("explore_summary_table")
          )
        )
      ),

      # ==========================================================================
      # MULTI-PATH TAB
      # ==========================================================================
      tabItem(
        tabName = "paths",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-sliders-h"></i> Algorithm Parameters'),
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            sliderInput("K", "Maximum Model Size (K):", 1, 20, 5, 1),
            numericInput("delta", "AIC Threshold (δ):", 2, 0.1, step = 0.5, min = 0),
            numericInput("L", "Frontier Size (L):", 50, 10, step = 10, min = 10),
            hr(),
            actionButton(
              "run_paths",
              HTML('<i class="fas fa-rocket"></i> Run Multi-Path Analysis'),
              class = "btn-primary btn-block"
            ),
            hr(),
            div(
              style = sprintf("padding: 15px; background: %s; border-radius: 12px; border-left: 4px solid %s;", app_colors$hover, app_colors$info),
              h5(tags$i(class = "fas fa-info-circle", style = "margin-right: 8px;"), "Parameter Guidance:"),
              uiOutput("parameter_suggestions")
            )
          ),
          box(
            title = HTML('<i class="fas fa-chart-line"></i> Analysis Results'),
            status = "success",
            solidHeader = TRUE,
            width = 8,
            conditionalPanel(
              condition = "output.paths_complete",
              verbatimTextOutput("paths_output"),
              hr(),
              plotOutput("plot_aic_step", height = "400px")
            ),
            conditionalPanel(
              condition = "!output.paths_complete",
              div(
                style = "text-align: center; padding: 100px 40px;",
                tags$i(class = "fas fa-arrow-left", style = sprintf("font-size: 80px; color: %s; opacity: 0.3;", app_colors$primary)),
                h3("Configure parameters and run analysis", style = "margin-top: 30px; opacity: 0.6;")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = HTML('<i class="fas fa-list-alt"></i> All Discovered Models'),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("all_models_table")
          )
        )
      ),

      # ==========================================================================
      # STABILITY TAB
      # ==========================================================================
      tabItem(
        tabName = "stability",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-cog"></i> Stability Parameters'),
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            sliderInput("B", "Bootstrap Resamples (B):", 10, 100, 30, 5),
            selectInput(
              "resample_type",
              "Resampling Method:",
              choices = c(
                "Bootstrap Sampling" = "bootstrap",
                "Subsample (80%)" = "subsample"
              )
            ),
            sliderInput("tau", "Stability Threshold (τ):", 0, 1, 0.6, 0.05),
            hr(),
            actionButton(
              "run_stability",
              HTML('<i class="fas fa-sync-alt"></i> Run Stability Analysis'),
              class = "btn-primary btn-block"
            )
          ),
          box(
            title = HTML('<i class="fas fa-chart-area"></i> Stability Results'),
            status = "success",
            solidHeader = TRUE,
            width = 8,
            conditionalPanel(
              condition = "output.stability_complete",
              verbatimTextOutput("stability_output"),
              hr(),
              plotOutput("plot_stability", height = "400px")
            ),
            conditionalPanel(
              condition = "!output.stability_complete",
              div(
                style = "text-align: center; padding: 100px 40px;",
                tags$i(class = "fas fa-exclamation-triangle", style = sprintf("font-size: 80px; color: %s; opacity: 0.3;", app_colors$warning)),
                h3("Please run Multi-Path Analysis first", style = "margin-top: 30px; opacity: 0.6;")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = HTML('<i class="fas fa-table"></i> Stability Rankings'),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("stability_table")
          )
        )
      ),

      # ==========================================================================
      # PLAUSIBLE MODELS TAB
      # ==========================================================================
      tabItem(
        tabName = "plausible",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-filter"></i> Model Selection Criteria'),
            status = "warning",
            solidHeader = TRUE,
            width = 4,
            numericInput("Delta", "AIC Tolerance (Δ):", 2, 0.1, step = 0.5, min = 0),
            checkboxInput("use_stability", "Use stability filtering", TRUE),
            conditionalPanel(
              condition = "input.use_stability",
              numericInput("tau_plausible", "Stability Threshold (τ):", 0.6, 0, 1, 0.05)
            ),
            checkboxInput("refit_models", "Refit selected models", TRUE),
            hr(),
            actionButton(
              "run_plausible",
              HTML('<i class="fas fa-star"></i> Select Plausible Models'),
              class = "btn-primary btn-block"
            )
          ),
          box(
            title = HTML('<i class="fas fa-trophy"></i> Selected Models'),
            status = "success",
            solidHeader = TRUE,
            width = 8,
            conditionalPanel(
              condition = "output.plausible_complete",
              verbatimTextOutput("plausible_output"),
              hr(),
              DTOutput("plausible_table")
            ),
            conditionalPanel(
              condition = "!output.plausible_complete",
              div(
                style = "text-align: center; padding: 100px 40px;",
                tags$i(class = "fas fa-hourglass-half", style = sprintf("font-size: 80px; color: %s; opacity: 0.3;", app_colors$danger)),
                h3("Complete previous analysis steps", style = "margin-top: 30px; opacity: 0.6;")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = HTML('<i class="fas fa-medal"></i> Variable Importance Ranking'),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("plot_importance", height = "450px")
          )
        )
      ),

      # ==========================================================================
      # COMPARISON TAB
      # ==========================================================================
      tabItem(
        tabName = "comparison",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-balance-scale"></i> Model Selection'),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(4, selectInput("compare_model1", "First Model:", choices = NULL)),
              column(4, selectInput("compare_model2", "Second Model:", choices = NULL)),
              column(4, selectInput("compare_model3", "Third Model (Optional):", choices = c("None" = "none")))
            ),
            actionButton(
              "run_comparison",
              HTML('<i class="fas fa-chart-bar"></i> Compare Models'),
              class = "btn-primary btn-block"
            )
          )
        ),
        fluidRow(
          box(
            title = HTML('<i class="fas fa-table"></i> Comparison Results'),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("comparison_table")
          )
        )
      ),

      # ==========================================================================
      # DIAGNOSTICS TAB
      # ==========================================================================
      tabItem(
        tabName = "diagnostics",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-stethoscope"></i> Select Model for Diagnostics'),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("diagnostic_model", "Choose Model:", choices = NULL),
            actionButton(
              "run_diagnostics",
              HTML('<i class="fas fa-search"></i> Run Diagnostics'),
              class = "btn-primary"
            )
          )
        ),
        fluidRow(
          box(
            title = HTML('<i class="fas fa-chart-scatter"></i> Residuals vs Fitted'),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("diagnostic_residuals", height = "450px")
          ),
          box(
            title = HTML('<i class="fas fa-chart-line"></i> Q-Q Plot'),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotOutput("diagnostic_qq", height = "450px")
          )
        )
      ),

      # ==========================================================================
      # PLOTS TAB
      # ==========================================================================
      tabItem(
        tabName = "plots",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-chart-pie"></i> Comprehensive Dashboard'),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            conditionalPanel(
              condition = "output.plausible_complete",
              plotOutput("plot_dashboard", height = "750px")
            ),
            conditionalPanel(
              condition = "!output.plausible_complete",
              div(
                style = "text-align: center; padding: 150px 40px;",
                tags$i(class = "fas fa-chart-area", style = sprintf("font-size: 100px; color: %s; opacity: 0.2;", app_colors$primary)),
                h3("Complete analysis to view dashboard", style = "margin-top: 40px; opacity: 0.5;")
              )
            )
          )
        )
      ),

      # ==========================================================================
      # REPORTS TAB
      # ==========================================================================
      tabItem(
        tabName = "reports",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-file-alt"></i> Report Configuration'),
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            textInput("report_title", "Report Title:", "Multi-Path Analysis Report"),
            textInput("report_author", "Author Name:", ""),
            selectInput(
              "report_format",
              "Output Format:",
              choices = c("Plain Text" = "txt", "Markdown" = "md")
            ),
            hr(),
            h5(tags$i(class = "fas fa-list-check", style = "margin-right: 8px;"), "Include Sections:"),
            checkboxInput("report_data", "Data Summary", TRUE),
            checkboxInput("report_paths", "Multi-Path Results", TRUE),
            checkboxInput("report_stability", "Stability Analysis", TRUE),
            checkboxInput("report_plausible", "Plausible Models", TRUE),
            hr(),
            actionButton(
              "generate_report",
              HTML('<i class="fas fa-file-export"></i> Generate Report'),
              class = "btn-success btn-block"
            )
          ),
          box(
            title = HTML('<i class="fas fa-info-circle"></i> Report Status'),
            status = "info",
            solidHeader = TRUE,
            width = 6,
            uiOutput("report_status"),
            hr(),
            conditionalPanel(
              condition = "output.report_generated",
              downloadButton(
                "download_report",
                HTML('<i class="fas fa-download"></i> Download Report'),
                class = "btn-primary btn-block"
              )
            )
          )
        )
      ),

      # ==========================================================================
      # DOWNLOADS TAB
      # ==========================================================================
      tabItem(
        tabName = "download",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-download"></i> Export Data & Results'),
            status = "success",
            solidHeader = TRUE,
            width = 12,
            h4(tags$i(class = "fas fa-file-csv", style = "margin-right: 10px;"), "CSV Exports:"),
            fluidRow(
              column(
                4,
                downloadButton("download_paths", HTML('<i class="fas fa-project-diagram"></i> Multi-Path Models'), class = "btn-primary btn-block")
              ),
              column(
                4,
                downloadButton("download_stability", HTML('<i class="fas fa-chart-line"></i> Stability Results'), class = "btn-primary btn-block")
              ),
              column(
                4,
                downloadButton("download_plausible", HTML('<i class="fas fa-star"></i> Plausible Models'), class = "btn-primary btn-block")
              )
            ),
            hr(),
            h4(tags$i(class = "fas fa-image", style = "margin-right: 10px;"), "Plot Exports:"),
            fluidRow(
              column(
                4,
                downloadButton("download_plot_stability", HTML('<i class="fas fa-chart-area"></i> Stability Plot'), class = "btn-success btn-block")
              ),
              column(
                4,
                downloadButton("download_plot_dashboard", HTML('<i class="fas fa-chart-pie"></i> Dashboard Plot'), class = "btn-success btn-block")
              ),
              column(
                4,
                downloadButton("download_plot_importance", HTML('<i class="fas fa-medal"></i> Importance Plot'), class = "btn-success btn-block")
              )
            )
          )
        )
      ),

      # ==========================================================================
      # ABOUT TAB
      # ==========================================================================
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = HTML('<i class="fas fa-info-circle"></i> About multipathaic'),
            status = "info",
            solidHeader = TRUE,
            width = 12,
            div(
              style = "text-align: center; padding: 30px;",
              tags$i(class = "fas fa-rocket", style = sprintf("font-size: 100px; color: %s; margin-bottom: 30px;", app_colors$primary)),
              h2("multipathaic Professional", style = sprintf("color: %s; font-weight: 800; margin-bottom: 15px;", app_colors$primary)),
              h4("Multi-Path Variable Selection with AIC and Stability Analysis", style = "font-weight: 400; margin-bottom: 40px;")
            ),
            hr(),
            fluidRow(
              column(
                6,
                div(
                  style = sprintf("padding: 30px; background: %s; border-radius: 16px; height: 100%%;", app_colors$hover),
                  h4(tags$i(class = "fas fa-star", style = "margin-right: 10px;"), "Key Features:"),
                  tags$ul(
                    style = "line-height: 2.2; font-size: 14px;",
                    tags$li(tags$b("Multi-path forward selection"), " with AIC criterion"),
                    tags$li(tags$b("Stability analysis"), " via bootstrap/subsample"),
                    tags$li(tags$b("Plausible model identification"), " with filtering"),
                    tags$li(tags$b("Professional visualizations"), " and dashboards"),
                    tags$li(tags$b("Comprehensive reporting"), " system"),
                    tags$li(tags$b("Interactive data exploration"), " tools")
                  )
                )
              ),
              column(
                6,
                div(
                  style = sprintf("padding: 30px; background: %s; border-radius: 16px; height: 100%%;", app_colors$hover),
                  h4(tags$i(class = "fas fa-check-circle", style = "margin-right: 10px;"), "Perfect For:"),
                  tags$ul(
                    style = "line-height: 2.2; font-size: 14px;",
                    tags$li(tags$b("High-dimensional data"), " analysis"),
                    tags$li(tags$b("Model uncertainty"), " quantification"),
                    tags$li(tags$b("Reproducible research"), " workflows"),
                    tags$li(tags$b("Academic publications"), " and reports"),
                    tags$li(tags$b("Predictive modeling"), " projects"),
                    tags$li(tags$b("Statistical consulting"), " work")
                  )
                )
              )
            ),
            hr(),
            div(
              style = sprintf(
                "text-align: center; padding: 35px;
                background: linear-gradient(135deg, %s 0%%, %s 100%%);
                border-radius: 16px; color: white; margin-top: 30px;",
                app_colors$primary, app_colors$accent
              ),
              h4(
                tags$i(class = "fas fa-university", style = "margin-right: 12px;"),
                "Auburn University",
                style = "color: white; margin-bottom: 15px; font-weight: 700;"
              ),
              p(
                tags$i(class = "fas fa-code", style = "margin-right: 8px;"),
                "Version 3.0.0 Professional | December 2025",
                style = "color: white; margin: 0; font-size: 15px; font-weight: 600;"
              )
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {

  # ============================================================================
  # REACTIVE VALUES
  # ============================================================================
  rv <- reactiveValues(
    data = NULL,
    data_raw = NULL,
    data_cleaned = NULL,
    X = NULL,
    y = NULL,
    paths_result = NULL,
    stability_result = NULL,
    plausible_result = NULL,
    importance_result = NULL,
    all_models = NULL,
    filters = list(),
    outliers = NULL,
    activity_log = list(),
    report_content = NULL,
    diagnostic_model = NULL,
    comparison_data = NULL
  )

  # ============================================================================
  # UTILITY FUNCTIONS
  # ============================================================================
  add_activity <- function(msg) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    rv$activity_log <- c(
      list(paste0("[", timestamp, "] ", msg)),
      rv$activity_log
    )
    if (length(rv$activity_log) > 20) {
      rv$activity_log <- rv$activity_log[1:20]
    }
  }

  # ============================================================================
  # HEADER OUTPUTS
  # ============================================================================
  output$current_time_display <- renderText({
    invalidateLater(1000, session)
    format(Sys.time(), "%H:%M:%S")
  })

  # ============================================================================
  # DASHBOARD VALUE BOXES
  # ============================================================================
  output$dash_observations <- renderValueBox({
    valueBox(
      value = if(!is.null(rv$data)) formatC(nrow(rv$data), big.mark=",", format="d") else "—",
      subtitle = "Observations",
      icon = icon("database", lib = "font-awesome"),
      color = "aqua"
    )
  })

  output$dash_predictors <- renderValueBox({
    valueBox(
      value = if(!is.null(rv$X)) ncol(rv$X) else "—",
      subtitle = "Predictors",
      icon = icon("sliders-h", lib = "font-awesome"),
      color = "green"
    )
  })

  output$dash_models_found <- renderValueBox({
    valueBox(
      value = if(!is.null(rv$plausible_result)) nrow(rv$plausible_result) else "—",
      subtitle = "Plausible Models",
      icon = icon("star", lib = "font-awesome"),
      color = "yellow"
    )
  })

  output$dash_status <- renderValueBox({
    status_text <- if(!is.null(rv$plausible_result)) "Complete ✓"
    else if(!is.null(rv$paths_result)) "In Progress"
    else if(!is.null(rv$data)) "Data Loaded"
    else "Not Started"

    status_color <- if(!is.null(rv$plausible_result)) "green"
    else if(!is.null(rv$paths_result)) "yellow"
    else if(!is.null(rv$data)) "aqua"
    else "red"

    valueBox(
      value = status_text,
      subtitle = "Analysis Status",
      icon = icon("tasks", lib = "font-awesome"),
      color = status_color
    )
  })

  # ============================================================================
  # ACTIVITY LOG
  # ============================================================================
  output$activity_log_display <- renderUI({
    if (length(rv$activity_log) == 0) {
      return(
        div(
          style = "text-align: center; padding: 60px 30px;",
          tags$i(class = "fas fa-clipboard-list", style = sprintf("font-size: 60px; color: %s; opacity: 0.2;", app_colors$primary)),
          p("No activities yet", style = "margin-top: 20px; opacity: 0.5; font-weight: 600;")
        )
      )
    }

    lapply(rv$activity_log, function(entry) {
      div(
        style = sprintf(
          "padding: 12px 16px; margin: 8px 0;
          background: %s; border-left: 4px solid %s;
          border-radius: 8px; font-size: 13px;
          transition: all 0.3s ease;",
          app_colors$hover, app_colors$primary
        ),
        tags$i(class = "fas fa-check-circle", style = sprintf("color: %s; margin-right: 10px;", app_colors$success)),
        entry
      )
    })
  })

  # ============================================================================
  # SMART RECOMMENDATIONS
  # ============================================================================
  output$smart_recommendations_display <- renderUI({
    recommendations <- list()

    if (is.null(rv$data)) {
      recommendations[[1]] <- div(
        class = "alert alert-info",
        tags$i(class = "fas fa-lightbulb", style = "margin-right: 10px;"),
        tags$b("Get Started:"), " Load your data from the Data Management tab"
      )
    } else if (is.null(rv$paths_result)) {
      recommendations[[1]] <- div(
        class = "alert alert-warning",
        tags$i(class = "fas fa-rocket", style = "margin-right: 10px;"),
        tags$b("Next Step:"), " Run Multi-Path Analysis with your data"
      )
    } else if (is.null(rv$stability_result)) {
      recommendations[[1]] <- div(
        class = "alert alert-warning",
        tags$i(class = "fas fa-sync-alt", style = "margin-right: 10px;"),
        tags$b("Recommended:"), " Run Stability Analysis to assess variable robustness"
      )
    } else if (is.null(rv$plausible_result)) {
      recommendations[[1]] <- div(
        class = "alert alert-warning",
        tags$i(class = "fas fa-star", style = "margin-right: 10px;"),
        tags$b("Almost Done:"), " Select plausible models using AIC and stability"
      )
    } else {
      recommendations[[1]] <- div(
        class = "alert alert-success",
        tags$i(class = "fas fa-check-double", style = "margin-right: 10px;"),
        tags$b("Complete:"), " Analysis finished! Review results and generate reports"
      )
      recommendations[[2]] <- div(
        class = "alert alert-info",
        tags$i(class = "fas fa-download", style = "margin-right: 10px;"),
        tags$b("Export:"), " Download your results from the Downloads tab"
      )
    }

    if (length(recommendations) == 0) {
      recommendations[[1]] <- div(
        class = "alert alert-info",
        tags$i(class = "fas fa-info-circle", style = "margin-right: 10px;"),
        "Follow the analysis workflow step by step"
      )
    }

    return(recommendations)
  })

  # ============================================================================
  # SIDEBAR STATS
  # ============================================================================
  output$sidebar_stats <- renderUI({
    if (!is.null(rv$data)) {
      tagList(
        div(
          style = sprintf("color: %s; font-size: 13px; line-height: 2;", app_colors$text_dark),
          p(
            tags$i(class = "fas fa-database", style = sprintf("margin-right: 8px; width: 20px; color: %s;", app_colors$primary)),
            tags$b(formatC(nrow(rv$data), big.mark=",", format="d")), " observations"
          ),
          p(
            tags$i(class = "fas fa-chart-bar", style = sprintf("margin-right: 8px; width: 20px; color: %s;", app_colors$primary)),
            tags$b(ncol(rv$X)), " predictors"
          ),
          if (!is.null(rv$plausible_result)) {
            p(
              tags$i(class = "fas fa-star", style = sprintf("margin-right: 8px; width: 20px; color: %s;", app_colors$warning)),
              tags$b(nrow(rv$plausible_result)), " plausible models"
            )
          }
        )
      )
    } else {
      p("Load data to begin", style = sprintf("color: %s; text-align: center; margin: 20px 0;", app_colors$text_light))
    }
  })

  # ============================================================================
  # QUICK ACTIONS
  # ============================================================================
  observeEvent(input$start_analysis, {
    updateTabItems(session, "sidebar", "data")
  })

  observeEvent(input$load_example, {
    updateRadioButtons(session, "data_source", selected = "builtin")
    updateSelectInput(session, "builtin_dataset", selected = "mtcars")
    click("preview_data")
  })

  observeEvent(input$quick_load, {
    updateTabItems(session, "sidebar", "data")
  })

  observeEvent(input$quick_clean, {
    updateTabItems(session, "sidebar", "cleaning")
  })

  observeEvent(input$quick_paths, {
    if (!is.null(rv$X)) {
      updateTabItems(session, "sidebar", "paths")
    } else {
      showNotification("Please load data first!", type = "warning", duration = 3)
    }
  })

  observeEvent(input$quick_stability, {
    if (!is.null(rv$paths_result)) {
      updateTabItems(session, "sidebar", "stability")
    } else {
      showNotification("Please run Multi-Path Analysis first!", type = "warning", duration = 3)
    }
  })

  observeEvent(input$quick_plausible, {
    if (!is.null(rv$paths_result)) {
      updateTabItems(session, "sidebar", "plausible")
    } else {
      showNotification("Please complete previous analysis steps!", type = "warning", duration = 3)
    }
  })

  observeEvent(input$quick_report, {
    updateTabItems(session, "sidebar", "reports")
  })

  # ============================================================================
  # DATA PREVIEW
  # ============================================================================
  observeEvent(input$preview_data, {
    tryCatch({
      if (input$data_source == "builtin") {
        if (input$builtin_dataset == "mtcars") {
          data(mtcars, envir = environment())
          rv$data_raw <- mtcars
        } else if (input$builtin_dataset == "iris") {
          data(iris, envir = environment())
          rv$data_raw <- iris
        } else if (input$builtin_dataset == "boston") {
          if (requireNamespace("MASS", quietly = TRUE)) {
            data(Boston, package = "MASS", envir = environment())
            rv$data_raw <- Boston
          } else {
            showNotification("MASS package required for Boston dataset", type = "error")
            return()
          }
        } else if (input$builtin_dataset == "diabetes") {
          if (requireNamespace("MASS", quietly = TRUE)) {
            data(Pima.tr, package = "MASS", envir = environment())
            rv$data_raw <- Pima.tr
          } else {
            showNotification("MASS package required for Diabetes dataset", type = "error")
            return()
          }
        }
      } else {
        req(input$file_upload)
        rv$data_raw <- read.csv(
          input$file_upload$datapath,
          header = input$header,
          sep = input$sep,
          dec = input$dec,
          stringsAsFactors = FALSE
        )
      }

      # Update select inputs
      updateSelectInput(session, "response_var", choices = names(rv$data_raw))
      updateSelectInput(session, "predictor_vars", choices = names(rv$data_raw))
      updateSelectInput(session, "explore_var", choices = names(rv$data_raw))
      updateSelectInput(session, "explore_predictor", choices = names(rv$data_raw))
      updateSelectInput(session, "filter_var", choices = names(rv$data_raw))

      add_activity(sprintf("Data loaded: %s (%d rows, %d cols)",
                           if(input$data_source == "builtin") input$builtin_dataset else "CSV",
                           nrow(rv$data_raw), ncol(rv$data_raw)))

      showNotification("Data loaded successfully!", type = "message", duration = 3)

    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 5)
    })
  })

  # ============================================================================
  # DATA OUTPUTS
  # ============================================================================
  output$raw_data_preview <- renderDT({
    req(rv$data_raw)
    datatable(
      head(rv$data_raw, 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'frtip',
        language = list(search = "Search:")
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })

  output$n_obs <- renderValueBox({
    valueBox(
      value = if(!is.null(rv$data)) formatC(nrow(rv$data), big.mark=",", format="d") else "—",
      subtitle = "Total Observations",
      icon = icon("database", lib = "font-awesome"),
      color = "aqua"
    )
  })

  output$n_pred <- renderValueBox({
    valueBox(
      value = if(!is.null(rv$X)) ncol(rv$X) else "—",
      subtitle = "Predictor Variables",
      icon = icon("sliders-h", lib = "font-awesome"),
      color = "green"
    )
  })

  output$response_type <- renderValueBox({
    response_type <- if(!is.null(rv$y)) {
      if(input$family == "binomial") "Binary" else "Continuous"
    } else "—"

    valueBox(
      value = response_type,
      subtitle = "Response Type",
      icon = icon("chart-bar", lib = "font-awesome"),
      color = "yellow"
    )
  })

  output$data_status <- renderValueBox({
    status_text <- if(!is.null(rv$data)) "✓ Ready" else "⚠ No Data"
    status_color <- if(!is.null(rv$data)) "green" else "red"

    valueBox(
      value = status_text,
      subtitle = "Data Status",
      icon = icon("check-circle", lib = "font-awesome"),
      color = status_color
    )
  })

  # ============================================================================
  # LOAD AND PREPARE DATA
  # ============================================================================
  observeEvent(input$load_data, {
    req(input$response_var, input$predictor_vars)

    withProgress(message = 'Processing data...', value = 0, {
      tryCatch({
        # Get data source
        df <- if (!is.null(rv$data_cleaned)) rv$data_cleaned else rv$data_raw
        req(df)
        incProgress(0.2, detail = "Extracting variables")

        # Extract response and predictors
        y_raw <- df[[input$response_var]]
        X_raw <- df[, input$predictor_vars, drop = FALSE]

        # Remove NA if requested
        if (input$remove_na) {
          complete_cases <- complete.cases(cbind(y_raw, X_raw))
          y_raw <- y_raw[complete_cases]
          X_raw <- X_raw[complete_cases, , drop = FALSE]
          incProgress(0.1, detail = "Removing missing values")
        }

        incProgress(0.2, detail = "Converting variables")

        # Handle categorical variables
        if (input$handle_categorical) {
          for (col in names(X_raw)) {
            if (is.factor(X_raw[[col]]) || is.character(X_raw[[col]])) {
              X_raw[[col]] <- as.numeric(factor(X_raw[[col]]))
            }
          }
        }

        # Convert to matrix
        X_matrix <- as.matrix(X_raw)
        if (is.null(colnames(X_matrix))) {
          colnames(X_matrix) <- input$predictor_vars
        }

        incProgress(0.2, detail = "Preprocessing")

        # Scale if requested
        if (input$scale_vars) {
          X_matrix <- scale(X_matrix)
        }

        # Handle binary response
        if (input$family == "binomial") {
          y_raw <- as.numeric(factor(y_raw)) - 1
        }

        # Store results
        rv$data <- cbind(data.frame(response = y_raw), as.data.frame(X_matrix))
        names(rv$data)[1] <- input$response_var
        rv$X <- X_matrix
        rv$y <- y_raw

        incProgress(0.3, detail = "Finalizing")

        add_activity(sprintf("Data prepared: %d observations, %d predictors",
                             nrow(rv$data), ncol(rv$X)))

        showNotification("Data prepared successfully!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Error preparing data:", e$message), type = "error", duration = 5)
      })
    })
  })

  # ============================================================================
  # DATA QUALITY REPORT
  # ============================================================================
  output$data_quality <- renderPrint({
    req(rv$data)

    cat("╔═══════════════════════════════════════════════════════════╗\n")
    cat("║           DATA QUALITY REPORT                             ║\n")
    cat("╚═══════════════════════════════════════════════════════════╝\n\n")

    cat(sprintf("📊 Total Observations: %d\n", nrow(rv$data)))
    cat(sprintf("📈 Total Variables: %d\n", ncol(rv$data)))
    cat(sprintf("🎯 Response Variable: %s\n", input$response_var))
    cat(sprintf("🔧 Model Family: %s\n\n",
                if(input$family == "gaussian") "Linear (Gaussian)" else "Logistic (Binomial)"))

    if (input$family == "binomial") {
      tab <- table(rv$y)
      cat("Binary Response Distribution:\n")
      cat(sprintf("  • Class 0: %d observations (%.1f%%)\n",
                  tab[1], 100*tab[1]/sum(tab)))
      cat(sprintf("  • Class 1: %d observations (%.1f%%)\n",
                  tab[2], 100*tab[2]/sum(tab)))

      if (min(tab)/max(tab) < 0.3) {
        cat("\n⚠️  Warning: Imbalanced classes detected\n")
      }
    }

    cat("\n✓ Data is ready for analysis!\n")
    cat("═══════════════════════════════════════════════════════════\n")
  })

  # ============================================================================
  # DATA CLEANING - FILTERS
  # ============================================================================
  output$filter_var_type <- reactive({
    req(rv$data_raw, input$filter_var)
    if (is.numeric(rv$data_raw[[input$filter_var]])) "numeric" else "categorical"
  })
  outputOptions(output, "filter_var_type", suspendWhenHidden = FALSE)

  output$filter_range_ui <- renderUI({
    req(rv$data_raw, input$filter_var)
    var_data <- rv$data_raw[[input$filter_var]]
    if (is.numeric(var_data)) {
      sliderInput(
        "filter_range",
        "Select Range:",
        min = floor(min(var_data, na.rm = TRUE)),
        max = ceiling(max(var_data, na.rm = TRUE)),
        value = c(floor(min(var_data, na.rm = TRUE)),
                  ceiling(max(var_data, na.rm = TRUE)))
      )
    }
  })

  observeEvent(input$add_filter, {
    req(rv$data_raw, input$filter_var)
    filter_info <- list(variable = input$filter_var)
    rv$filters[[length(rv$filters) + 1]] <- filter_info
    add_activity(sprintf("Filter added: %s", input$filter_var))
    showNotification("Filter added successfully!", type = "message", duration = 2)
  })

  observeEvent(input$clear_filters, {
    rv$filters <- list()
    add_activity("All filters cleared")
    showNotification("Filters cleared!", type = "message", duration = 2)
  })

  output$active_filters <- renderPrint({
    if (length(rv$filters) == 0) {
      cat("No active filters\n")
    } else {
      cat("Active Filters:\n")
      cat(strrep("─", 40), "\n")
      for (i in seq_along(rv$filters)) {
        cat(sprintf("%d. Variable: %s\n", i, rv$filters[[i]]$variable))
      }
    }
  })

  # ============================================================================
  # APPLY CLEANING
  # ============================================================================
  observeEvent(input$apply_cleaning, {
    req(rv$data_raw)

    withProgress(message = 'Cleaning data...', value = 0, {
      tryCatch({
        df <- rv$data_raw
        original_rows <- nrow(df)

        # Handle missing values
        if (input$missing_method == "remove") {
          df <- na.omit(df)
          incProgress(0.3, detail = "Removing missing values")
        }

        # Remove duplicates
        if (input$remove_duplicates) {
          df <- df[!duplicated(df), ]
          incProgress(0.3, detail = "Removing duplicates")
        }

        incProgress(0.4, detail = "Finalizing")

        rv$data_cleaned <- df
        rows_removed <- original_rows - nrow(df)

        add_activity(sprintf("Data cleaned: %d rows removed, %d rows remaining",
                             rows_removed, nrow(df)))

        showNotification(
          sprintf("Cleaning complete! %d rows removed.", rows_removed),
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        showNotification(paste("Error cleaning data:", e$message), type = "error", duration = 5)
      })
    })
  })

  output$cleaned_data_preview <- renderDT({
    req(rv$data_cleaned)
    datatable(
      head(rv$data_cleaned, 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'frtip'
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })

  # ============================================================================
  # EXPLORATION PLOTS
  # ============================================================================
  output$explore_histogram <- renderPlotly({
    req(rv$data, input$explore_var)

    p <- ggplot(rv$data, aes_string(x = input$explore_var)) +
      geom_histogram(
        bins = 30,
        fill = app_colors$primary,
        alpha = 0.7,
        color = app_colors$accent
      ) +
      theme_minimal() +
      labs(
        title = paste("Distribution of", input$explore_var),
        x = input$explore_var,
        y = "Frequency"
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold")
      )

    ggplotly(p)
  })

  output$explore_correlation <- renderPlotly({
    req(rv$X)

    cor_mat <- cor(rv$X, use = "complete.obs")

    plot_ly(
      x = colnames(cor_mat),
      y = colnames(cor_mat),
      z = cor_mat,
      type = "heatmap",
      colorscale = list(
        c(0, app_colors$danger),
        c(0.5, "white"),
        c(1, app_colors$primary)
      ),
      colorbar = list(title = "Correlation")
    ) %>%
      layout(
        title = "Correlation Heatmap",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })

  output$explore_scatter <- renderPlotly({
    req(rv$data, input$explore_predictor)

    p <- ggplot(rv$data, aes_string(x = input$explore_predictor, y = input$response_var)) +
      geom_point(
        color = app_colors$primary,
        alpha = 0.6,
        size = 2
      ) +
      geom_smooth(
        method = "lm",
        color = app_colors$danger,
        se = TRUE
      ) +
      theme_minimal() +
      labs(
        title = paste(input$response_var, "vs", input$explore_predictor),
        x = input$explore_predictor,
        y = input$response_var
      ) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold")
      )

    ggplotly(p)
  })

  output$explore_summary_table <- renderDT({
    req(rv$data)

    summary_stats <- data.frame(
      Variable = names(rv$data),
      Mean = sapply(rv$data, mean, na.rm = TRUE),
      SD = sapply(rv$data, sd, na.rm = TRUE),
      Min = sapply(rv$data, min, na.rm = TRUE),
      Max = sapply(rv$data, max, na.rm = TRUE)
    )

    datatable(
      summary_stats,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(2:5, 3)
  })

  # ============================================================================
  # MULTI-PATH ANALYSIS
  # ============================================================================
  output$parameter_suggestions <- renderUI({
    req(rv$X)
    n <- nrow(rv$X)
    p <- ncol(rv$X)
    suggested_K <- min(p, max(3, floor(p/2)))

    tagList(
      p(
        sprintf("💡 Suggested K: %d (half of %d predictors)", suggested_K, p),
        style = "margin: 5px 0; font-size: 13px;"
      ),
      p(
        sprintf("💡 Default δ: 2 (standard AIC threshold)"),
        style = "margin: 5px 0; font-size: 13px;"
      ),
      p(
        sprintf("📊 Dataset: n = %d, p = %d", n, p),
        style = "margin: 5px 0; font-size: 13px; font-weight: 600;"
      )
    )
  })

  observeEvent(input$run_paths, {
    req(rv$X, rv$y)

    withProgress(message = 'Running Multi-Path Analysis...', value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Initializing algorithm")

        rv$paths_result <- build_paths(
          X = rv$X,
          y = rv$y,
          family = input$family,
          K = input$K,
          delta = input$delta,
          L = input$L,
          verbose = FALSE
        )

        incProgress(0.5, detail = "Extracting models")

        rv$all_models <- extract_all_models(rv$paths_result)

        incProgress(0.3, detail = "Finalizing")

        add_activity(sprintf("Multi-Path complete: %d models discovered", nrow(rv$all_models)))

        showNotification(
          sprintf("Multi-Path analysis complete! Found %d models.", nrow(rv$all_models)),
          type = "message",
          duration = 4
        )

      }, error = function(e) {
        showNotification(paste("Error in Multi-Path:", e$message), type = "error", duration = 5)
      })
    })
  })

  output$paths_complete <- reactive({
    !is.null(rv$paths_result)
  })
  outputOptions(output, "paths_complete", suspendWhenHidden = FALSE)

  output$paths_output <- renderPrint({
    req(rv$paths_result)
    print(rv$paths_result)
  })

  output$plot_aic_step <- renderPlot({
    req(rv$paths_result)
    plot_aic_by_step(rv$paths_result)
  })

  output$all_models_table <- renderDT({
    req(rv$all_models)
    datatable(
      rv$all_models,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'frtip'
      ),
      rownames = FALSE
    ) %>%
      formatRound('aic', 2)
  })

  # ============================================================================
  # STABILITY ANALYSIS
  # ============================================================================
  observeEvent(input$run_stability, {
    req(rv$X, rv$y)

    withProgress(message = 'Running Stability Analysis...', value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Initializing resampling")

        rv$stability_result <- stability(
          X = rv$X,
          y = rv$y,
          family = input$family,
          B = input$B,
          resample_type = input$resample_type,
          K = input$K,
          verbose = FALSE
        )

        incProgress(0.8, detail = "Computing stability scores")

        add_activity(sprintf("Stability analysis complete: %d resamples", input$B))

        showNotification(
          "Stability analysis complete!",
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        showNotification(paste("Error in Stability:", e$message), type = "error", duration = 5)
      })
    })
  })

  output$stability_complete <- reactive({
    !is.null(rv$stability_result)
  })
  outputOptions(output, "stability_complete", suspendWhenHidden = FALSE)

  output$stability_output <- renderPrint({
    req(rv$stability_result)
    print(rv$stability_result)
  })

  output$plot_stability <- renderPlot({
    req(rv$stability_result)
    plot_stability(rv$stability_result, threshold = input$tau)
  })

  output$stability_table <- renderDT({
    req(rv$stability_result)

    df <- data.frame(
      Variable = names(rv$stability_result$pi),
      Stability = round(rv$stability_result$pi, 4)
    )
    df <- df[order(-df$Stability), ]

    datatable(
      df,
      options = list(pageLength = 15, dom = 'frtip'),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Stability',
        background = styleColorBar(df$Stability, app_colors$primary),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  # ============================================================================
  # PLAUSIBLE MODELS
  # ============================================================================
  observeEvent(input$run_plausible, {
    req(rv$paths_result)

    withProgress(message = 'Selecting Plausible Models...', value = 0, {
      tryCatch({
        incProgress(0.3, detail = "Applying selection criteria")

        rv$plausible_result <- plausible_models(
          path_result = rv$paths_result,
          stability_result = if(input$use_stability) rv$stability_result else NULL,
          Delta = input$Delta,
          tau = if(input$use_stability) input$tau_plausible else NULL,
          refit = input$refit_models,
          X = if(input$refit_models) rv$X else NULL,
          y = if(input$refit_models) rv$y else NULL
        )

        incProgress(0.4, detail = "Computing importance rankings")

        if (!is.null(rv$stability_result)) {
          rv$importance_result <- variable_importance_ranking(
            rv$paths_result,
            rv$stability_result
          )
        }

        # Update model selection dropdowns
        updateSelectInput(session, "diagnostic_model", choices = rv$plausible_result$model_id)
        updateSelectInput(session, "compare_model1", choices = rv$plausible_result$model_id)
        updateSelectInput(session, "compare_model2", choices = rv$plausible_result$model_id)
        updateSelectInput(session, "compare_model3",
                          choices = c("None" = "none", rv$plausible_result$model_id))

        incProgress(0.3, detail = "Finalizing")

        add_activity(sprintf("Found %d plausible models", nrow(rv$plausible_result)))

        showNotification(
          sprintf("Selection complete! %d plausible models identified.",
                  nrow(rv$plausible_result)),
          type = "message",
          duration = 4
        )

      }, error = function(e) {
        showNotification(paste("Error selecting models:", e$message), type = "error", duration = 5)
      })
    })
  })

  output$plausible_complete <- reactive({
    !is.null(rv$plausible_result)
  })
  outputOptions(output, "plausible_complete", suspendWhenHidden = FALSE)

  output$plausible_output <- renderPrint({
    req(rv$plausible_result)
    print(rv$plausible_result)
  })

  output$plausible_table <- renderDT({
    req(rv$plausible_result)

    display_cols <- c("model_id", "size", "variables", "aic", "delta_aic", "avg_stability")

    datatable(
      rv$plausible_result[, display_cols],
      options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'),
      rownames = FALSE
    ) %>%
      formatRound(c('aic', 'delta_aic', 'avg_stability'), 4)
  })

  output$plot_importance <- renderPlot({
    req(rv$importance_result)
    plot_variable_importance(
      rv$importance_result,
      top_n = min(15, nrow(rv$importance_result))
    )
  })

  # ============================================================================
  # MODEL COMPARISON
  # ============================================================================
  observeEvent(input$run_comparison, {
    req(input$compare_model1, input$compare_model2, rv$plausible_result)

    models_to_compare <- c(input$compare_model1, input$compare_model2)
    if (input$compare_model3 != "none") {
      models_to_compare <- c(models_to_compare, input$compare_model3)
    }

    rv$comparison_data <- rv$plausible_result[
      rv$plausible_result$model_id %in% models_to_compare,
    ]

    add_activity(sprintf("Compared %d models", length(models_to_compare)))
    showNotification("Models compared successfully!", type = "message", duration = 2)
  })

  output$comparison_table <- renderDT({
    req(rv$comparison_data)

    datatable(
      rv$comparison_data[, c("model_id", "size", "variables", "aic", "avg_stability")],
      options = list(dom = 't', scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(c("aic", "avg_stability"), 4)
  })

  # ============================================================================
  # DIAGNOSTICS
  # ============================================================================
  observeEvent(input$run_diagnostics, {
    req(input$diagnostic_model, rv$plausible_result)

    model_idx <- which(rv$plausible_result$model_id == input$diagnostic_model)
    rv$diagnostic_model <- rv$plausible_result$fitted_model[[model_idx]]

    add_activity(sprintf("Running diagnostics: %s", input$diagnostic_model))
    showNotification("Diagnostics computed!", type = "message", duration = 2)
  })

  output$diagnostic_residuals <- renderPlot({
    req(rv$diagnostic_model)
    plot(rv$diagnostic_model, which = 1)
  })

  output$diagnostic_qq <- renderPlot({
    req(rv$diagnostic_model)
    plot(rv$diagnostic_model, which = 2)
  })

  # ============================================================================
  # ADVANCED PLOTS
  # ============================================================================
  output$plot_dashboard <- renderPlot({
    req(rv$paths_result, rv$stability_result, rv$plausible_result)
    plot_model_dashboard(rv$paths_result, rv$stability_result, rv$plausible_result)
  })

  # ============================================================================
  # REPORT GENERATION
  # ============================================================================
  observeEvent(input$generate_report, {
    req(rv$data)

    withProgress(message = 'Generating report...', value = 0, {
      report_lines <- c(
        strrep("=", 70),
        input$report_title,
        if(input$report_author != "") paste0("Author: ", input$report_author),
        paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        strrep("=", 70),
        ""
      )

      incProgress(0.2)

      if (input$report_data) {
        report_lines <- c(
          report_lines,
          "DATA SUMMARY",
          strrep("-", 70),
          sprintf("Observations: %d", nrow(rv$data)),
          sprintf("Predictors: %d", ncol(rv$X)),
          sprintf("Response: %s", input$response_var),
          sprintf("Family: %s", input$family),
          ""
        )
      }

      incProgress(0.3)

      if (input$report_paths && !is.null(rv$paths_result)) {
        report_lines <- c(
          report_lines,
          "MULTI-PATH RESULTS",
          strrep("-", 70),
          sprintf("Models explored: %d", nrow(rv$all_models)),
          sprintf("Parameters: K=%d, delta=%.2f, L=%d",
                  input$K, input$delta, input$L),
          ""
        )
      }

      incProgress(0.3)

      if (input$report_stability && !is.null(rv$stability_result)) {
        report_lines <- c(
          report_lines,
          "STABILITY ANALYSIS",
          strrep("-", 70),
          sprintf("Resamples: %d", input$B),
          sprintf("Method: %s", input$resample_type),
          ""
        )
      }

      incProgress(0.2)

      if (input$report_plausible && !is.null(rv$plausible_result)) {
        report_lines <- c(
          report_lines,
          "PLAUSIBLE MODELS",
          strrep("-", 70),
          sprintf("Models selected: %d", nrow(rv$plausible_result)),
          sprintf("AIC tolerance: %.2f", input$Delta),
          ""
        )
      }

      rv$report_content <- paste(report_lines, collapse = "\n")
      add_activity("Report generated successfully")
      showNotification("Report ready for download!", type = "message", duration = 3)
    })
  })

  output$report_generated <- reactive({
    !is.null(rv$report_content)
  })
  outputOptions(output, "report_generated", suspendWhenHidden = FALSE)

  output$report_status <- renderUI({
    if (!is.null(rv$report_content)) {
      div(
        class = "alert alert-success",
        tags$i(class = "fas fa-check-circle", style = "margin-right: 10px;"),
        tags$b("Success!"), " Report generated and ready for download."
      )
    } else {
      div(
        class = "alert alert-info",
        tags$i(class = "fas fa-info-circle", style = "margin-right: 10px;"),
        "Configure report options and click Generate."
      )
    }
  })

  # ============================================================================
  # DOWNLOADS
  # ============================================================================
  output$download_paths <- downloadHandler(
    filename = function() { paste0("multipath_models_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$all_models)
      write.csv(rv$all_models, file, row.names = FALSE)
    }
  )

  output$download_stability <- downloadHandler(
    filename = function() { paste0("stability_results_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$stability_result)
      df <- data.frame(
        Variable = names(rv$stability_result$pi),
        Stability = rv$stability_result$pi
      )
      write.csv(df, file, row.names = FALSE)
    }
  )

  output$download_plausible <- downloadHandler(
    filename = function() { paste0("plausible_models_", Sys.Date(), ".csv") },
    content = function(file) {
      req(rv$plausible_result)
      write.csv(
        rv$plausible_result[, c("model_id", "size", "variables", "aic", "avg_stability")],
        file,
        row.names = FALSE
      )
    }
  )

  output$download_report <- downloadHandler(
    filename = function() { paste0("report_", Sys.Date(), ".", input$report_format) },
    content = function(file) {
      req(rv$report_content)
      writeLines(rv$report_content, file)
    }
  )

  output$download_plot_stability <- downloadHandler(
    filename = function() { paste0("stability_plot_", Sys.Date(), ".png") },
    content = function(file) {
      req(rv$stability_result)
      png(file, width = 3000, height = 2400, res = 300)
      plot_stability(rv$stability_result, threshold = input$tau)
      dev.off()
    }
  )

  output$download_plot_dashboard <- downloadHandler(
    filename = function() { paste0("dashboard_plot_", Sys.Date(), ".png") },
    content = function(file) {
      req(rv$paths_result, rv$stability_result, rv$plausible_result)
      png(file, width = 4200, height = 3000, res = 300)
      plot_model_dashboard(rv$paths_result, rv$stability_result, rv$plausible_result)
      dev.off()
    }
  )

  output$download_plot_importance <- downloadHandler(
    filename = function() { paste0("importance_plot_", Sys.Date(), ".png") },
    content = function(file) {
      req(rv$importance_result)
      png(file, width = 3000, height = 2400, res = 300)
      plot_variable_importance(rv$importance_result, top_n = min(15, nrow(rv$importance_result)))
      dev.off()
    }
  )

  # ============================================================================
  # HELP MODAL
  # ============================================================================
  observeEvent(input$help_btn, {
    showModal(
      modalDialog(
        title = HTML('<i class="fas fa-question-circle"></i> Quick Help Guide'),
        size = "l",
        div(
          style = "padding: 20px;",
          h4(tags$i(class = "fas fa-rocket", style = "margin-right: 10px;"), "Getting Started"),
          tags$ol(
            style = "line-height: 2; font-size: 14px;",
            tags$li(tags$b("Data Management:"), " Load your dataset or use built-in examples"),
            tags$li(tags$b("Data Cleaning:"), " Handle missing values and outliers"),
            tags$li(tags$b("Exploration:"), " Visualize distributions and correlations"),
            tags$li(tags$b("Multi-Path:"), " Run forward selection algorithm (K=3-5 recommended)"),
            tags$li(tags$b("Stability:"), " Assess variable importance via resampling (B=30+)"),
            tags$li(tags$b("Plausible Models:"), " Select final models using AIC and stability"),
            tags$li(tags$b("Reports:"), " Generate and download comprehensive analysis reports")
          ),
          hr(),
          h4(tags$i(class = "fas fa-lightbulb", style = "margin-right: 10px;"), "Pro Tips"),
          tags$ul(
            style = "line-height: 2; font-size: 14px;",
            tags$li("Use ", tags$b("K = 3-5"), " for initial exploration, increase for complex models"),
            tags$li("Increase ", tags$b("B (resamples)"), " to 50-100 for more robust stability estimates"),
            tags$li("Enable ", tags$b("stability filtering"), " when selecting plausible models"),
            tags$li("Compare ", tags$b("top 2-3 models"), " using the Comparison tab"),
            tags$li("Always check ", tags$b("diagnostic plots"), " for model assumptions"),
            tags$li("Export results to CSV for further analysis in other tools")
          ),
          hr(),
          div(
            class = "alert alert-info",
            tags$i(class = "fas fa-info-circle", style = "margin-right: 10px;"),
            "For more details, visit the ", tags$b("About"), " tab or consult the package documentation."
          )
        ),
        easyClose = TRUE,
        footer = modalButton(HTML('<i class="fas fa-times"></i> Close'))
      )
    )
  })
}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================

shinyApp(ui = ui, server = server)

