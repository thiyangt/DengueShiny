library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)
library(denguedatahub)
library(plotly)

# -----------------------------
# DATA
# -----------------------------
data("srilanka_weekly_data")
dengue_df <- srilanka_weekly_data

dengue_df <- dengue_df %>%
  mutate(
    year <- factor(year),
    start.date = mdy(start.date),
    end.date = mdy(end.date),
    week = as.numeric(week)
  )

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  
  titlePanel("Sri Lanka Dengue Surveillance Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "district",
        "Select District:",
        choices = sort(unique(dengue_df$district)),
        selected = "Colombo"
      ),
      
      selectInput(
        "n_weeks",
        "Select recent period:",
        choices = c(
          "Last 4 weeks" = 4,
          "Last 8 weeks" = 8,
          "Last 12 weeks" = 12,
          "Last 24 weeks" = 24,
          "Custom"
        ),
        selected = 4
      ),
      
      conditionalPanel(
        condition = "input.n_weeks == 'Custom'",
        numericInput(
          "custom_weeks",
          "Enter number of weeks:",
          value = 12,
          min = 1,
          max = 200
        )
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Time Series",
          plotOutput("ts_plot", height = "500px")
        ),
        
        tabPanel(
          "Heatmap (Year × Week)",
          plotOutput("heatmap_plot", height = "600px")
        ),
        
        tabPanel(
          "Year-wise Summary",
          tableOutput("summary_table")
        ),
        
        tabPanel(
          "Top 10 Weeks",
          DTOutput("top10_table")
        ),
        
        tabPanel(
          "Recent 10 Weeks",
          DTOutput("toprecent10_table")
        ),
        
        tabPanel(
          "Recent Burden",
          h3(textOutput("latest_cases"))
        ),
        
        tabPanel(
          "Raw Data",
          DTOutput("raw_table")
        )
        
      )
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------

server <- function(input, output) {
  
  # -----------------------------
  # FILTER DATA
  # -----------------------------
  
  district_data <- reactive({
    dengue_df %>%
      filter(district == input$district) %>%
      arrange(year, week) %>%
      mutate(week_id = row_number())
  })
  
  # -----------------------------
  # TIME SERIES
  # -----------------------------
  
  output$ts_plot <- renderPlot({
    
    plot_data <- district_data()
    
    year_breaks <- plot_data %>%
      group_by(year) %>%
      summarise(pos = min(week_id), .groups = "drop")
    
    ggplot(plot_data, aes(x = week_id, y = cases, col=year)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1) +
      scale_color_viridis_c() +
      scale_x_continuous(
        breaks = year_breaks$pos,
        labels = year_breaks$year
      ) +
      labs(
        title = paste("Weekly Dengue Cases -", input$district),
        x = "Year",
        y = "Cases"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # -----------------------------
  # HEATMAP (YEAR × WEEK)
  # -----------------------------
  
  output$heatmap_plot <- renderPlot({
    
    plot_data <- district_data() %>%
      mutate(
        risk_class = cut(
          cases,
          breaks = c(-Inf, 200, 400, 600, 800, 1000, 2000, 3000, Inf),
          labels = c(
            "0–50",
            "51–100",
            "101–150",
            "151–200",
            "201–250",
            "251–300",
            "301–400",
            ">400"
          ),
          right = TRUE
        )
      )
    
    ggplot(plot_data, aes(x = week, y = factor(year), fill = risk_class)) +
      geom_tile(color = "grey90", linewidth = 0.2) +
      
      scale_fill_viridis_d(option = "C", drop = FALSE) +
      
      scale_x_continuous(
        breaks = seq(1, 52, by = 1)
      ) +
      
      labs(
        title = paste("Dengue Risk Heatmap (Binned) -", input$district),
        x = "Week",
        y = "Year",
        fill = "Risk Level"
      ) +
      
      theme_minimal(base_size = 13) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 10)
      )
  })
  # -----------------------------
  # YEAR-WISE SUMMARY
  # -----------------------------
  
  output$summary_table <- renderTable({
    
    district_data() %>%
      mutate(year = factor(year)) %>%
      group_by(year) %>%
      summarise(
        Total = sum(cases, na.rm = TRUE),
        Mean = round(mean(cases, na.rm = TRUE), 2),
        Median = median(cases, na.rm = TRUE),
        SD = round(sd(cases, na.rm = TRUE), 2),
        Max = max(cases, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # -----------------------------
  # TOP 10 WEEKS
  # -----------------------------
  
  output$top10_table <- renderDT({
    
    district_data() %>%
      arrange(desc(cases)) %>%
      slice_head(n = 10) %>%
      select(year, week, cases, start.date, end.date)
  })
  
  # -----------------------------
  # RECENT 10 WEEKS
  # -----------------------------
  
  output$toprecent10_table <- renderDT({
    
    district_data() %>%
      arrange(desc(start.date)) %>%
      slice_head(n = 10) %>%
      select(year, week, cases, start.date, end.date)
  })
  
  # -----------------------------
  # LAST N WEEKS SUMMARY
  # -----------------------------
  
  output$latest_cases <- renderText({
    
    n <- if (input$n_weeks == "Custom") {
      input$custom_weeks
    } else {
      as.numeric(input$n_weeks)
    }
    
    latest_data <- district_data() %>%
      arrange(desc(start.date)) %>%
      slice_head(n = n)
    
    paste0(
      "Last ", n, " Weeks Summary\n",
      "Total Cases: ", sum(latest_data$cases, na.rm = TRUE), "\n",
      "Average Weekly Cases: ", round(mean(latest_data$cases, na.rm = TRUE), 2)
    )
  })
  
  # -----------------------------
  # RAW DATA
  # -----------------------------
  
  output$raw_table <- renderDT({
    district_data()
  })
  
}

# -----------------------------
# RUN APP
# -----------------------------

shinyApp(ui, server)