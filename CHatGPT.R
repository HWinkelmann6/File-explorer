# Load required libraries
install.packages("ggplot2")
library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for the Shiny app
ui <- fluidPage(
  
  # App title
  titlePanel("CSV File Upload and Data Visualization"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # File input for CSV upload
      fileInput("file", "Choose CSV File", 
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Checkbox to display the first 6 rows of the file
      checkboxInput("show_data", "Show first 6 rows", value = TRUE)
    ),
    
    # Main panel to display data and plots
    mainPanel(
      # Display data table
      tableOutput("data_preview"),
      
      # Output containers for plots
      uiOutput("plots")
    )
  )
)

# Define server logic required to read the file and generate plots
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file) # Ensure a file is uploaded before proceeding
    read.csv(input$file$datapath, stringsAsFactors = TRUE)
  })
  
  # Output for displaying the first 6 rows of the file
  output$data_preview <- renderTable({
    req(input$file)
    if (input$show_data) {
      head(data(), 6)
    }
  })
  
  # Generate plots based on variable types
  output$plots <- renderUI({
    req(input$file)
    df <- data()
    
    # Identify numeric and factor variables
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    factor_vars <- names(df)[sapply(df, is.factor)]
    
    # Create plot outputs for numeric variables
    numeric_plots <- lapply(numeric_vars, function(var) {
      plotOutput(outputId = paste0("plot_", var))
    })
    
    # Create plot outputs for factor variables
    factor_plots <- lapply(factor_vars, function(var) {
      plotOutput(outputId = paste0("plot_", var))
    })
    
    # Combine all plots into a tagList
    do.call(tagList, c(numeric_plots, factor_plots))
  })
  
  # Observe and render plots for numeric variables
  observe({
    req(input$file)
    df <- data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    
    lapply(numeric_vars, function(var) {
      output[[paste0("plot_", var)]] <- renderPlot({
        ggplot(df, aes_string(x = var)) +
          geom_histogram(binwidth = 30, fill = "skyblue", color = "black") +
          labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
          theme_minimal()
      })
    })
  })
  
  # Observe and render plots for factor variables
  observe({
    req(input$file)
    df <- data()
    factor_vars <- names(df)[sapply(df, is.factor)]
    
    lapply(factor_vars, function(var) {
      output[[paste0("plot_", var)]] <- renderPlot({
        ggplot(df, aes_string(x = var)) +
          geom_bar(fill = "orange", color = "black") +
          labs(title = paste("Bar Chart of", var), x = var, y = "Count") +
          theme_minimal()
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
