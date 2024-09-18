# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)  # Load lubridate for flexible date parsing
library(scales)

# Load the data with stringsAsFactors = FALSE to prevent automatic factor conversion
data <- read.csv("SNYTrends.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Combine Year and Month to create a date-like format for plotting
data$YearMonth <- paste(data$Year, data$Month, "01", sep = "-")  # Create YYYY-MM-DD format with day set as 01
data$YearMonth <- ymd(data$YearMonth)  # Convert to Date object

# Ensure MOE and Sample columns are correctly read as numeric (remove any stray characters like % if necessary)
data$MOE <- as.numeric(gsub("[^0-9.]", "", data$MOE))  # Remove % and convert to numeric
data$Sample <- as.numeric(data$Sample)  # Ensure Sample is numeric

# Ensure Voters values are consistent and match expected filter inputs
data$Voters <- tolower(data$Voters)  # Convert to lowercase to match filter conditions

# UI
ui <- fluidPage(
  titlePanel("Trends Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_variable",
        label = "Select a percentage to display:",
        choices = colnames(data)[grepl("NYRT|NYWD|USRT|USWD|Fav|Unf", colnames(data))],
        selected = colnames(data)[1]
      ),
      selectInput(
        inputId = "voter_filter",
        label = "Filter by Voter Type:",
        choices = c("All", "Likely", "Registered"),
        selected = "All"
      ),
      sliderInput(
        inputId = "moe_filter",
        label = "Select Margin of Error (MOE) Range:",
        min = 3,
        max = 5,
        value = c(3, 5),
        step = 0.1
      ),
      sliderInput(
        inputId = "sample_filter",
        label = "Select Sample Size Range:",
        min = 600,
        max = 1300,
        value = c(600, 1300),
        step = 50
      ),
      dateRangeInput(
        inputId = "date_filter",
        label = "Select Date Range:",
        start = min(data$YearMonth),
        end = max(data$YearMonth),
        min = min(data$YearMonth),
        max = max(data$YearMonth)
      )
    ),
    mainPanel(
      plotOutput("trendPlot"),
      tableOutput("filteredData")  # Display filtered data for verification
    )
  )
)

# Server
server <- function(input, output) {
  # Create a reactive expression for the filtered data
  filtered_data <- reactive({
    data %>%
      filter(
        (input$voter_filter == "All") |
          (input$voter_filter == "Likely" & Voters == "likely") |
          (input$voter_filter == "Registered" & Voters == "registered"),
        MOE >= input$moe_filter[1] & MOE <= input$moe_filter[2],  # Filter by MOE range
        Sample >= input$sample_filter[1] & Sample <= input$sample_filter[2],  # Filter by sample size range
        YearMonth >= input$date_filter[1] & YearMonth <= input$date_filter[2]  # Filter by selected date range
      )
  })
  
  # Output the filtered data to confirm filtering
  output$filteredData <- renderTable({
    head(filtered_data(), 20)  # Display the first 20 rows of the filtered data for verification
  })
  
  output$trendPlot <- renderPlot({
    # Get the filtered data
    plot_data <- filtered_data() %>%
      select(YearMonth, all_of(input$selected_variable)) %>%
      mutate(across(all_of(input$selected_variable), ~ as.numeric(gsub("%", "", .)) / 100))
    
    # Check if there's data to plot
    if (nrow(plot_data) == 0) {
      showNotification("No data available for the selected filters.", type = "warning")
      return(NULL)
    }
    
    # Plot the filtered data using YearMonth for the X-axis
    ggplot(plot_data, aes(x = YearMonth, y = .data[[input$selected_variable]])) +
      geom_line(aes(group = 1), na.rm = FALSE) +
      geom_point(na.rm = TRUE) +
      scale_x_date(date_labels = "%Y", date_breaks = "1 year") +  # Display only years on the X-axis
      labs(
        title = paste("Trend of", input$selected_variable, "Over Time"),
        x = "Year",
        y = input$selected_variable
      ) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
