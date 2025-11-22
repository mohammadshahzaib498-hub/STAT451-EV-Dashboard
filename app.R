#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(tidyr)
library(scales)
library(DT)
library(data.table)

# Utility Nicknames (Required for Tab 3)
utility_nicknames <- c(
  "PUGET SOUND ENERGY INC||CITY OF TACOMA - (WA)" = "PSE & Tacoma",
  "PUGET SOUND ENERGY INC" = "PSE",
  "CITY OF SEATTLE - (WA)|CITY OF TACOMA - (WA)" = "Seattle & Tacoma",
  "BONNEVILLE POWER ADMINISTRATION||PUD NO 1 OF CLARK COUNTY - (WA)" = "BPA & Clark County",
  "BONNEVILLE POWER ADMINISTRATION||CITY OF TACOMA - (WA)||PENINSULA LIGHT COMPANY" = "BPA & Tacoma & Peninsula",
  "PUGET SOUND ENERGY INC||PUD NO 1 OF WHATCOM COUNTY" = "PSE & Whatcom County",
  "BONNEVILLE POWER ADMINISTRATION||AVISTA CORP||INLAND POWER & LIGHT COMPANY" = "BPA & Avista & Inland",
  "BONNEVILLE POWER ADMINISTRATION||PUD 1 OF SNOHOMISH COUNTY" = "BPA & Snohomish County"
)

# ==========================================
# DATA LOADING FUNCTION
# ==========================================

load_data <- function() {
  # Try to find the file in multiple locations
  possible_paths <- c(
    "Electric_Vehicle_Population_Data.xlsx", # Same directory
    "~/Desktop/STAT 451/Electric_Vehicle_Population_Data.xlsx", # Absolute path
    file.path(getwd(), "Electric_Vehicle_Population_Data.xlsx") # Current working directory
  )
  
  data_path <- NULL
  for (path in possible_paths) {
    if (file.exists(path)) {
      data_path <- path
      break
    }
  }
  
  if (is.null(data_path)) {
    stop("Data file not found. Searched in:\n",
         paste(possible_paths, collapse = "\n"),
         "\n\nPlease ensure 'Electric_Vehicle_Population_Data.xlsx' is accessible.")
  }
  
  message("Loading data from: ", data_path)
  ev_data <- read_excel(data_path, sheet = 1)
  
  # ==========================================
  # DATA CLEANING: Remove rows with ANY missing values
  # ==========================================
  
  original_rows <- nrow(ev_data)
  
  # Remove rows with any NA values
  ev_data_clean <- ev_data %>%
    drop_na() # This removes any row that has at least one NA value
  
  cleaned_rows <- nrow(ev_data_clean)
  removed_rows <- original_rows - cleaned_rows
  
  message(sprintf("Data cleaning complete:"))
  message(sprintf(" - Original rows: %s", scales::comma(original_rows)))
  message(sprintf(" - Rows removed: %s", scales::comma(removed_rows)))
  message(sprintf(" - Final rows: %s", scales::comma(cleaned_rows)))
  
  return(ev_data_clean)
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "EV Market Trends Analysis (STAT 451)"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sbmenu",
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Data Exploration", tabName = "data", icon = icon("table")),
      menuItem("Range Trend Analysis", tabName = "range_trend", icon = icon("chart-line")),
      menuItem("County EV Composition", tabName = "county_comp", icon = icon("chart-bar"))
    ),
    
    # Inputs for Tab 2: Data Exploration
    conditionalPanel(
      condition = "input.sbmenu == 'data'",
      uiOutput("boxplot_column_ui")
    ),
    
    # Inputs for Tab 3: Range Trend Analysis
    conditionalPanel(
      condition = "input.sbmenu == 'range_trend'",
      selectInput(
        inputId = "selected_make",
        label = "Filter by Vehicle Brand:",
        choices = NULL,
        selected = "All",
        multiple = TRUE
      ),
      
      sliderInput(
        inputId = "min_range",
        label = "Minimum Average Range (Miles):",
        min = 0, max = 300, value = 50, step = 10
      )
    ),
    
    # Inputs for Tab 4: County EV Composition
    conditionalPanel(
      condition = "input.sbmenu == 'county_comp'",
      sliderInput(
        inputId = "top_n_counties",
        label = "Number of Top Counties to Display:",
        min = 5, max = 20, value = 10, step = 1
      ),
      selectInput(
        inputId = "selected_type",
        label = "Filter by Vehicle Type:",
        choices = c("All", "Battery Electric Vehicle (BEV)", "Plug-in Hybrid Electric Vehicle (PHEV)"),
        selected = "All"
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab 1 — Overview
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Project Focus: Electric Vehicle Adoption", width = 12,
                    status = "primary", solidHeader = TRUE,
                    p("This dashboard explores key trends in the electric vehicle (EV) population dataset for Washington State, focusing on how EV range is evolving and how adoption varies geographically."),
                    h4("Team Members"),
                    p("• Member 1: Shahzaib Mohammad (Overview)"),
                    p("• Member 2: Zachary Wu (Data Exploration)"),
                    p("• Member 3: Sangey Richen (Modeling)"),
                    p("• Member 4: Li-An Song (Visualization)")
                )
              )
      ),
      
      # Tab 2 — Data Exploration
      tabItem(tabName = "data",
              h2(tags$strong("Data Exploration and Quality Check")),
              fluidRow(
                box(title = "Distribution Plot", width = 12, status = "warning", solidHeader = TRUE,
                    p("Visualize the distribution, central tendency (median marked by a diamond), and outliers for the selected numeric column."),
                    plotlyOutput("numeric_boxplot", height = "400px")
                )
              ),
              fluidRow(
                box(title = "Missing Value Summary", width = 6, status = "danger", solidHeader = TRUE,
                    p("This table shows the count and percentage of missing values (NA) for each column, highlighting potential data quality issues."),
                    dataTableOutput("missing_summary")
                ),
                box(title = "Descriptive Statistics (Numeric Columns)", width = 6, status = "info", solidHeader = TRUE,
                    p("Key summary statistics for all numeric variables, useful for checking distributions and outliers."),
                    dataTableOutput("numeric_summary")
                )
              ),
              fluidRow(
                box(title = "Raw Data Viewer (First 100 Rows)", width = 12, status = "success", solidHeader = TRUE,
                    p("A searchable, paginated view of the raw dataset."),
                    dataTableOutput("raw_data_table")
                )
              )
      ),
      
      # Tab 3 — Range Trend Analysis
      tabItem(tabName = "range_trend",
              fluidRow(
                box(
                  title = tags$strong("Question: Average EV Range Trend by Utility Service Area"),
                  width = 12, status = "success", solidHeader = TRUE,
                  p("This visualization answers the question: For the Top Brands, how has the average Electric Range trended over the years, and how does the trend differ when comparing the various Electric Utility areas? Use the sidebar filters to refine the brands and the minimum range displayed.")
                )
              ),
              fluidRow(
                valueBoxOutput("avg_range_box", width = 6),
                valueBoxOutput("max_range_box", width = 6)
              ),
              fluidRow(
                box(
                  title = "Average Range by Vehicle Brand", width = 6,
                  plotOutput("range_trend_make_plot", height = "500px")
                ),
                box(
                  title = "Average Range by Electric Utility", width = 6,
                  plotOutput("range_trend_utility_plot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = "Aggregated Data Table", width = 12,
                  dataTableOutput("range_data_table")
                )
              )
      ),
      
      # Tab 4 — County EV Composition
      tabItem(tabName = "county_comp",
              fluidRow(
                box(
                  title = tags$strong("Question: EV Population Split (BEV vs. PHEV) by County"),
                  width = 12, status = "info", solidHeader = TRUE,
                  p("This visualization answers the question: How is the EV population composed of Battery Electric Vehicles (BEV) and Plug-in Hybrid Electric Vehicles (PHEV) across the most popular counties? Use the sidebar slider to select the number of top counties to display.")
                )
              ),
              fluidRow(
                box(
                  title = "EV Population Composition: Top Counties", width = 12,
                  plotlyOutput("county_comp_plot", height = "600px")
                )
              )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Load the comprehensive data frame once
  raw_data <- load_data()
  start_year <- 2015
  end_year <- 2019
  
  # Reactive data frame that performs the crucial initial filtering and conversion:
  filtered_data <- reactive({
    data <- raw_data %>%
      # Convert Electric Range to numeric, suppressing coercion warnings
      mutate(`Electric Range` = suppressWarnings(as.numeric(`Electric Range`))) %>%
      # Filter: must be a valid number (>0) and not NA (created from non-numeric text)
      filter(!is.na(`Electric Range`),
             `Electric Range` > 0)
    
    return(data)
  })
  
  # Reactive to identify numeric columns for dynamic UI updates
  numeric_cols <- reactive({
    filtered_data() %>%
      select(where(is.numeric)) %>%
      colnames()
  })
  
  # Dynamic UI for plot variable selection (replaces static selectInput in UI)
  output$boxplot_column_ui <- renderUI({
    req(numeric_cols())
    selectInput(
      inputId = "boxplot_column",
      label = tags$strong("Box Plot Variable Selection:"),
      choices = numeric_cols(),
      selected = if ("Electric Range" %in% numeric_cols()) "Electric Range" else numeric_cols()[1]
    )
  })
  
  # Update brand choices dynamically (Tab 3)
  observe({
    req(filtered_data())
    # Filter for makes that appear more than 10 times to avoid cluttering the selector
    valid_makes <- filtered_data() %>%
      count(`Make`) %>%
      filter(n > 10) %>%
      pull(`Make`)
    
    unique_makes <- sort(unique(valid_makes))
    
    updateSelectInput(
      session,
      "selected_make",
      choices = c("All", unique_makes),
      selected = "All"
    )
  })
  
  # --- TAB 2: DATA EXPLORATION REACTIVES ---
  
  # 1. Missing Value Summary Table
  output$missing_summary <- renderDataTable({
    data <- filtered_data()
    
    # Robustly calculate missing counts and percentages
    missing_summary_df <- data %>%
      summarise(across(everything(), ~sum(is.na(.)))) %>%
      pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count") %>%
      mutate(
        Total_Rows = nrow(data),
        `Missing %` = round((Missing_Count / Total_Rows) * 100, 2)
      ) %>%
      select(Column, `Missing Count` = Missing_Count, `Missing %`) %>%
      # Formatting for display
      mutate(
        `Missing Count` = scales::comma(`Missing Count`),
        `Missing %` = paste0(`Missing %`, "%")
      )
    
    datatable(
      missing_summary_df,
      options = list(pageLength = 10, dom = 't', ordering = FALSE),
      rownames = FALSE
    )
  })
  
  # 2. Numeric Descriptive Statistics Table
  output$numeric_summary <- renderDataTable({
    numeric_data <- filtered_data() %>%
      select(where(is.numeric))
    
    if (ncol(numeric_data) == 0) {
      return(data.frame(Message = "No numeric columns found.") %>% datatable(options = list(dom = 't'), rownames = FALSE))
    }
    
    # Use explicit names in summarise and pivot/separate for robust column handling
    summary_stats <- numeric_data %>%
      summarise(across(everything(), list(
        Count = ~ sum(!is.na(.)),
        Min = ~ min(., na.rm = TRUE),
        Max = ~ max(., na.rm = TRUE),
        Mean = ~ mean(., na.rm = TRUE),
        Median = ~ median(., na.rm = TRUE),
        SD = ~ sd(., na.rm = TRUE)
      ), .names = "{.fn}__{.col}")) %>%
      
      # Convert wide summary to long format for easier viewing and separation
      pivot_longer(everything(), names_to = "col", values_to = "value") %>%
      separate(col, into = c("Statistic", "Column"), sep = "__") %>%
      pivot_wider(names_from = Statistic, values_from = value) %>%
      
      # Format columns to 2 decimal places and use comma for Count
      mutate(across(c(Min, Max, Mean, Median, SD), ~ round(., 2))) %>%
      mutate(Count = scales::comma(Count))
    
    datatable(
      summary_stats,
      options = list(pageLength = 10, dom = 't', ordering = FALSE),
      rownames = FALSE
    )
  })
  
  # 3. Raw Data Table Viewer
  output$raw_data_table <- renderDataTable({
    filtered_data() %>%
      head(100) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  # 4. Box Plot for Numeric Distribution
  output$numeric_boxplot <- renderPlotly({
    req(input$boxplot_column)
    col_name <- input$boxplot_column
    
    # Data for the plot
    plot_data <- filtered_data() %>%
      select(column = !!sym(col_name)) %>%
      filter(!is.na(column))
    
    # Check if the selected column is numeric (safety check)
    if (!is.numeric(plot_data$column) || nrow(plot_data) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = paste(col_name, "is not a numeric variable for plotting or contains no data.")))
    }
    
    median_val <- median(plot_data$column, na.rm = TRUE)
    plot_title <- paste("Box Plot of", col_name)
    y_label <- col_name
    
    plot <- ggplot(plot_data, aes(y = column, x = 1)) +
      geom_boxplot(fill = "#f0ad4e", color = "#d9534f", outlier.color = "red", linewidth = 0.8) +
      
      # Add a point for the median for clarity (diamond shape)
      geom_point(y = median_val, x = 1, color = "black", size = 4, shape = 18) +
      labs(
        title = plot_title,
        y = y_label,
        x = ""
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(face = "bold", size = 16)
      )
    
    # Convert to plotly
    ggplotly(plot, tooltip = "y") %>%
      layout(
        yaxis = list(title = y_label),
        xaxis = list(showticklabels = FALSE, title = "")
      )
  })
  
  
  # --- TAB 3: RANGE TREND ANALYSIS (PLOT 1) REACTIVES ---
  
  # 1. Aggregated Data for Plot 1
  agg_data_plot1 <- reactive({
    req(input$selected_make)
    
    top_utilities <- filtered_data() %>%
      count(`Electric Utility`, sort = TRUE) %>%
      head(8) %>%
      pull(`Electric Utility`)
    
    selected_makes_filter <- input$selected_make
    if ("All" %in% selected_makes_filter || is.null(selected_makes_filter)) {
      # Identify top 20 most common makes from the FILTERED data
      top_n_makes <- filtered_data() %>%
        count(`Make`, sort = TRUE) %>%
        head(20) %>%
        pull(`Make`)
      selected_makes_filter <- top_n_makes
    }
    
    # Filter data down - ADDED YEAR FILTER FOR 2015-2020
    filtered_raw <- filtered_data() %>%
      filter(`Make` %in% selected_makes_filter,
             `Electric Utility` %in% top_utilities,
             `Model Year` >= start_year, # FILTER: Only 2015 and later
             `Model Year` <= end_year # FILTER: Only 2020 and earlier
      ) %>%
      mutate(electric_utility_nickname = recode(`Electric Utility`, !!!utility_nicknames))
    
    # Aggregation
    plot_data <- filtered_raw %>%
      group_by(`Model Year`, `Make`, electric_utility_nickname) %>%
      summarise(
        avg_range = mean(`Electric Range`, na.rm = TRUE),
        n_obs = n(),
        .groups = 'drop'
      ) %>%
      filter(n_obs > 1, !is.na(`Model Year`)) %>%
      
      # Apply UI Filters (Min Range)
      filter(avg_range >= input$min_range)
    
    return(plot_data)
  })
  
  # 3. Value Boxes for Plot 1
  output$avg_range_box <- renderValueBox({
    data <- agg_data_plot1()
    avg_range <- mean(data$avg_range, na.rm = TRUE)
    valueBox(
      value = tags$p(paste(round(avg_range), "miles"), style = "font-size: 200%;"),
      subtitle = "Overall Average Range (2015-2020, Filtered Data)",
      icon = icon("gauge-high"),
      color = "green"
    )
  })
  
  output$max_range_box <- renderValueBox({
    data <- agg_data_plot1()
    max_range <- max(data$avg_range, na.rm = TRUE)
    valueBox(
      value = tags$p(paste(round(max_range), "miles"), style = "font-size: 200%;"),
      subtitle = "Highest Observed Average Range (2015-2020, Filtered Data)",
      icon = icon("car-bolt"),
      color = "blue"
    )
  })
  
  # 4. Data Table for Plot 1
  output$range_data_table <- renderDataTable({
    agg_data_plot1() %>%
      select(`Model Year`, `Make`, `Utility` = electric_utility_nickname, `Avg Range` = avg_range, `Count` = n_obs) %>%
      mutate(`Avg Range` = round(`Avg Range`, 1)) %>%
      datatable(
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  # 5. Aggregated Data for Make Plot (Aggregates by Make only)
  agg_data_plot_make <- reactive({
    agg_data_plot1() %>%
      group_by(`Model Year`, `Make`) %>%
      summarise(
        avg_range = mean(avg_range, na.rm = TRUE),
        .groups = 'drop'
      )
  })
  
  # 6. Aggregated Data for Utility Plot (Aggregates by Utility only)
  agg_data_plot_utility <- reactive({
    agg_data_plot1() %>%
      group_by(`Model Year`, electric_utility_nickname) %>%
      summarise(
        avg_range = mean(avg_range, na.rm = TRUE),
        .groups = 'drop'
      )
  })
  
  # 7. Render Plot for Make comparison (Color aesthetic)
  output$range_trend_make_plot <- renderPlot({
    plot_data <- agg_data_plot_make()
    req(nrow(plot_data) > 0)
    
    # Force x-axis to show 2015-2020
    x_breaks <- seq(start_year, end_year, by = 1)
    
    plot_make <- ggplot(plot_data, aes(
      x = `Model Year`,
      y = avg_range,
      color = `Make`
    )) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = x_breaks, limits = c(start_year, end_year)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      labs(
        title = "Average Range Trend by Vehicle Brand",
        x = "Model Year",
        y = "Average Electric Range (Miles)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    return(plot_make)
  })
  
  # 8. Render Plot for Utility comparison (Linetype aesthetic)
  output$range_trend_utility_plot <- renderPlot({
    plot_data <- agg_data_plot_utility()
    req(nrow(plot_data) > 0)
    
    # Define all possible utilities and their styles
    all_utility_order <- c(
      "PSE & Tacoma", "PSE", "Seattle & Tacoma",
      "BPA & Clark County", "BPA & Tacoma & Peninsula",
      "PSE & Whatcom County", "BPA & Avista & Inland",
      "BPA & Snohomish County"
    )
    
    all_line_styles <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "22", "42")
    
    all_utility_colors <- c(
      "PSE & Tacoma" = "#E41A1C",
      "PSE" = "#377EB8",
      "Seattle & Tacoma" = "#4DAF4A",
      "BPA & Clark County" = "#984EA3",
      "BPA & Tacoma & Peninsula" = "#FF7F00",
      "PSE & Whatcom County" = "#A65628",
      "BPA & Avista & Inland" = "#F781BF",
      "BPA & Snohomish County" = "#999999"
    )
    
    # Force x-axis to show 2015-2020
    x_breaks <- seq(start_year, end_year, by = 1)
    
    plot_utility <- ggplot(plot_data, aes(
      x = `Model Year`,
      y = avg_range,
      linetype = electric_utility_nickname,
      color = electric_utility_nickname
    )) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      
      scale_linetype_manual(
        name = "Electric Utility",
        values = all_line_styles,
        breaks = all_utility_order
      ) +
      
      scale_color_manual(
        name = "Electric Utility",
        values = all_utility_colors,
        breaks = all_utility_order
      ) +
      
      scale_x_continuous(breaks = x_breaks, limits = c(start_year, end_year)) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
      labs(
        title = "Average Range Trend by Electric Utility",
        x = "Model Year",
        y = "Average Electric Range (Miles)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.key.width = unit(1.5, "cm")
      )
    
    return(plot_utility)
  })
  
  # --- TAB 4: COUNTY EV COMPOSITION (PLOT 2) REACTIVES ---
  
  # 1. Aggregated Data for Plot 2
  agg_data_plot2 <- reactive({
    req(input$top_n_counties, input$selected_type)
    n <- input$top_n_counties
    
    data_source <- filtered_data()
    
    # 1. Calculate total EV count per county
    county_totals <- data_source %>%
      filter(!is.na(`County`), `County` != "", `County` != "Other") %>%
      count(`County`, sort = TRUE, name = "total_count") %>%
      head(n)
    
    top_n_counties <- county_totals$`County`
    
    # 2. Filter raw data to top N counties and selected vehicle type
    plot_data <- data_source %>%
      filter(`County` %in% top_n_counties,
             `Electric Vehicle Type` %in% c("Battery Electric Vehicle (BEV)", "Plug-in Hybrid Electric Vehicle (PHEV)"))
    
    if (input$selected_type != "All") {
      plot_data <- plot_data %>%
        filter(`Electric Vehicle Type` == input$selected_type)
    }
    
    # 3. Aggregation and Ordering
    plot_data <- plot_data %>%
      group_by(`County`, `Electric Vehicle Type`) %>%
      summarise(vehicle_count = n(), .groups = 'drop') %>%
      left_join(county_totals, by = "County") %>%
      mutate(`County` = factor(`County`, levels = county_totals$`County`))
    
    return(plot_data)
  })
  
  # 2. Render Plotly Plot 2
  output$county_comp_plot <- renderPlotly({
    plot_data <- agg_data_plot2()
    req(nrow(plot_data) > 0)
    
    color_map <- c(
      "Battery Electric Vehicle (BEV)" = "#4daf4a",
      "Plug-in Hybrid Electric Vehicle (PHEV)" = "#377eb8"
    )
    
    plot2 <- ggplot(plot_data, aes(
      x = `County`,
      y = vehicle_count,
      fill = `Electric Vehicle Type`,
      text = paste0("County: ", `County`, "\nEV Type: ", `Electric Vehicle Type`, "\nCount: ", scales::comma(vehicle_count))
    )) +
      
      geom_bar(
        stat = "identity",
        position = position_dodge(width = 0.8),
        width = 0.7
      ) +
      
      scale_fill_manual(values = color_map, name = "EV Type") +
      scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
      
      labs(
        title = paste("EV Population Composition: Top", input$top_n_counties, "Counties"),
        x = "County (Ranked by Total EV Count)",
        y = "Total Number of Electric Vehicles",
        fill = "Vehicle Type"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(plot2, tooltip = "text") %>%
      layout(margin = list(b = 100))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
