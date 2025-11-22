Here is the content for your README.md file, provided line-by-line. This content is tailored to your Shiny application's purpose, data requirements, and file structure.

Markdown

# EV Market Trends Analysis (STAT 451)

---

## 🚀 Overview

This Shiny web application provides an interactive analysis of the **Electric Vehicle (EV) Population Data for Washington State**. The dashboard explores key trends in EV adoption, focusing primarily on the evolution of **Electric Range** over time and the geographic **composition of EV types (BEV vs. PHEV)** across different counties.

The app uses the `shinydashboard` package for its interface and leverages `ggplot2` and `plotly` for rich, interactive visualizations.

---

## 🛠️ Requirements & Setup

### 1. Data Source

The application **requires** the data file named `Electric_Vehicle_Population_Data.xlsx` to be present in one of the following locations:

1.  The same directory as the `app.R` (or your main R script).
2.  The absolute path `~/Desktop/STAT 451/Electric_Vehicle_Population_Data.xlsx`.
3.  The current R working directory (`getwd()`).

**Please ensure the data file is accessible in one of these locations before running the app.**

### 2. R Packages

To run the application, you must have the following R packages installed.

```r
# Install missing packages if necessary
if(!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, dplyr, ggplot2, plotly, readxl, tidyr, scales, DT, data.table)
```

### 3. ⚙️ Running the Application

To run the application, simply execute the following code in your **R console** from the directory containing the `app.R` file:

```r
shiny::runApp()
R

shiny::runApp()
📊 Dashboard Sections
The dashboard is organized into four main sections accessible via the sidebar menu:

1. Overview
Purpose: Provides a brief project description and lists the team members responsible for different sections.

2. Data Exploration
Purpose: Offers tools for a preliminary data quality check and exploration.

Features:

Missing Value Summary: A table showing NA counts and percentages for each column.

Descriptive Statistics: Summary statistics (min, max, mean, median, etc.) for all numeric columns.

Distribution Plot: An interactive box plot (powered by plotly) to visualize the distribution and outliers of a selected numeric column.

Raw Data Viewer: Displays the first 100 rows of the cleaned dataset.

3. Range Trend Analysis
Question: How has the average Electric Range trended over the years (2015-2020), and how does this trend differ by Vehicle Brand and Electric Utility service area?

Filters: Allows filtering by Vehicle Brand and setting a Minimum Average Range (Miles).

4. County EV Composition
Question: How is the EV population composed of Battery Electric Vehicles (BEV) versus Plug-in Hybrid Electric Vehicles (PHEV) across the most popular counties?

Filters: Allows selecting the Top N Counties to display and filtering by a specific Vehicle Type (BEV or PHEV).
