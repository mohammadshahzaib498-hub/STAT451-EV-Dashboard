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
```

### 4. 📊 Dashboard Sections
The dashboard is organized into four main sections accessible via the sidebar menu:

#### 1. Overview
Purpose: Provides a brief project description and lists the team members responsible for different sections.
<img width="911" height="663" alt="Screenshot 2025-11-22 at 3 11 07 PM" src="https://github.com/user-attachments/assets/3a045c62-c0c8-4366-a2e5-a5a1ab64f6e6" />

#### 2. Data Exploration
Purpose: Offers tools for a preliminary data quality check and exploration.

##### Features:

Missing Value Summary: A table showing NA counts and percentages for each column.
<img width="579" height="528" alt="Screenshot 2025-11-22 at 3 21 19 PM" src="https://github.com/user-attachments/assets/7a506034-df7e-4278-9439-c5db7093e7dc" />

Descriptive Statistics: Summary statistics (min, max, mean, median, etc.) for all numeric columns.
<img width="573" height="553" alt="Screenshot 2025-11-22 at 3 21 49 PM" src="https://github.com/user-attachments/assets/587bfe08-990b-4fac-a49c-9f50f3abcde5" />

Distribution Plot: An interactive box plot (powered by plotly) to visualize the distribution and outliers of a selected numeric column.
<img width="910" height="647" alt="Screenshot 2025-11-22 at 3 11 43 PM" src="https://github.com/user-attachments/assets/2fa317c3-25cd-4f5a-8930-f8cd36151ff6" />

Raw Data Viewer: Displays the first 100 rows of the cleaned dataset.

3. Range Trend Analysis
Question: How has the average Electric Range trended over the years (2015-2020), and how does this trend differ by Vehicle Brand and Electric Utility service area?

Filters: Allows filtering by Vehicle Brand and setting a Minimum Average Range (Miles).

4. County EV Composition
Question: How is the EV population composed of Battery Electric Vehicles (BEV) versus Plug-in Hybrid Electric Vehicles (PHEV) across the most popular counties?

Filters: Allows selecting the Top N Counties to display and filtering by a specific Vehicle Type (BEV or PHEV).
