# Load required packages
library(shiny)
library(ggplot2)
library(bslib)
library(vcd)  # For creating mosaic plots
library(DT)   # For displaying data tables
library(plotly)  # For interactive plots

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      .summary-table {
        background-color: #f2f2f2; 
        border: 2px solid #4CAF50;
        color: #333;
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),  # File input for uploading CSV
      checkboxGroupInput("plots", "Select Plots to Display:", 
                         choices = list("Scatterplot" = "scatterplot",
                                        "Histogram" = "histogram",
                                        "Bar Plot" = "barplot",
                                        "Mosaic Plot" = "mosaicplot",
                                        "Boxplot" = "boxplot"),
                         selected = c("scatterplot", "histogram")),  # Default selected plots
      uiOutput("var_select"),  # Dynamic UI for variable selection based on uploaded data
      sliderInput("bins", "Number of Bins for Histogram:", min = 5, max = 50, value = 30)  # Slider for histogram bins
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.plots.includes('scatterplot')",
        h3("Scatterplot"),
        plotlyOutput("scatterplot"),  # Interactive scatterplot using plotly
        downloadButton("downloadScatter", "Download Scatterplot")
      ),
      conditionalPanel(
        condition = "input.plots.includes('histogram')",
        h3("Histogram"),
        plotOutput("histogram"),  # Output for the histogram
        downloadButton("downloadHist", "Download Histogram")
      ),
      conditionalPanel(
        condition = "input.plots.includes('barplot')",
        h3("Bar Plot"),
        plotOutput("barplot"),  # Output for the bar plot
        downloadButton("downloadBar", "Download Bar Plot")
      ),
      conditionalPanel(
        condition = "input.plots.includes('mosaicplot')",
        h3("Mosaic Plot"),
        plotOutput("mosaicplot"),  # Output for the mosaic plot
        downloadButton("downloadMosaic", "Download Mosaic Plot")
      ),
      conditionalPanel(
        condition = "input.plots.includes('boxplot')",
        h3("Boxplot"),
        plotOutput("boxplot"),  # Output for the side-by-side boxplot
        downloadButton("downloadBox", "Download Boxplot")
      ),
      h3("Summary Statistics"),
      DTOutput("summary_table"),  # Styled summary statistics table
      h3("Data Overview"),
      DTOutput("data_table")      # Output for the data table, moved below all plots
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, header = TRUE, stringsAsFactors = FALSE)
      validate(need(ncol(df) > 0, "The uploaded file has no columns. Please check the file."))
      validate(need(nrow(df) > 0, "The uploaded file has no rows. Please check the file."))
      showNotification("File uploaded successfully!", type = "message")
      df <- na.omit(df)
      return(df)
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
  })
  
  # Dynamic UI for selecting variables based on uploaded data
  output$var_select <- renderUI({
    df <- data()
    req(df)
    
    num_vars <- names(df)[sapply(df, is.numeric)]
    qual_vars <- names(df)[sapply(df, function(col) is.factor(col) || is.character(col))]
    
    if (length(num_vars) == 0 || length(qual_vars) == 0) {
      showNotification("Data does not contain valid numeric or qualitative variables.", type = "error")
      return(NULL)
    }
    
    tagList(
      selectInput("x", "Scatterplot X Variable:", choices = num_vars, selected = num_vars[1]),
      selectInput("y", "Scatterplot Y Variable:", choices = num_vars, selected = if (length(num_vars) > 1) num_vars[2] else num_vars[1]),
      selectInput("hist_var", "Histogram:", choices = num_vars, selected = num_vars[1]),
      selectInput("bar_var", "Bar Plot:", choices = qual_vars, selected = qual_vars[1]),
      selectInput("mosaic_x", "Mosaic Plot X Variable:", choices = qual_vars, selected = qual_vars[1]),
      selectInput("mosaic_y", "Mosaic Plot Y Variable:", choices = qual_vars, selected = if (length(qual_vars) > 1) qual_vars[2] else qual_vars[1]),
      selectInput("boxplot_num", "Boxplot Numeric Variable:", choices = num_vars, selected = num_vars[1]),
      selectInput("boxplot_cat", "Boxplot Categorical Variable:", choices = qual_vars, selected = qual_vars[1])
    )
  })
  
  # Scatterplot output with plotly for interactivity
  output$scatterplot <- renderPlotly({
    req(data(), input$x, input$y)
    df <- data()
    p <- ggplot(df, aes_string(x = input$x, y = input$y)) + 
      geom_point() + 
      theme_minimal() +
      labs(x = input$x, y = input$y)
    ggplotly(p)  # Convert ggplot to plotly for interactive features
  })
  
  # Download handler for scatterplot with dynamic title
  output$downloadScatter <- downloadHandler(
    filename = function() {
      paste("scatterplot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      df <- data()
      p <- ggplot(df, aes_string(x = input$x, y = input$y)) + 
        geom_point() + 
        theme_minimal() +
        labs(
          title = paste("Scatterplot of", input$y, "by", input$x),
          x = input$x,
          y = input$y
        )
      print(p)
      dev.off()
    }
  )
  
  # Histogram output
  output$histogram <- renderPlot({
    req(data(), input$hist_var, input$bins)
    df <- data()
    ggplot(df, aes_string(x = input$hist_var)) +
      geom_histogram(bins = input$bins, fill = "steelblue", color = "black") + theme_minimal() +
      labs(x = input$hist_var, y = "Frequency", title = paste("Histogram of", input$hist_var))
  })
  
  # Download handler for histogram with dynamic title
  output$downloadHist <- downloadHandler(
    filename = function() {
      paste("histogram", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      df <- data()
      p <- ggplot(df, aes_string(x = input$hist_var)) +
        geom_histogram(bins = input$bins, fill = "steelblue", color = "black") + 
        theme_minimal() +
        labs(
          title = paste("Histogram of", input$hist_var),
          x = input$hist_var,
          y = "Frequency"
        )
      print(p)
      dev.off()
    }
  )
  
  # Bar plot output
  output$barplot <- renderPlot({
    req(data(), input$bar_var)
    df <- data()
    ggplot(df, aes_string(x = input$bar_var)) +
      geom_bar(fill = "orange", color = "black") + theme_minimal() +
      labs(x = input$bar_var, y = "Count", title = paste("Bar Plot of", input$bar_var))
  })
  
  # Download handler for bar plot with dynamic title
  output$downloadBar <- downloadHandler(
    filename = function() {
      paste("barplot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      df <- data()
      p <- ggplot(df, aes_string(x = input$bar_var)) +
        geom_bar(fill = "orange", color = "black") + 
        theme_minimal() +
        labs(
          title = paste("Bar Plot of", input$bar_var),
          x = input$bar_var,
          y = "Count"
        )
      print(p)
      dev.off()
    }
  )
  
  # Mosaic plot output
  output$mosaicplot <- renderPlot({
    req(data(), input$mosaic_x, input$mosaic_y)
    df <- data()
    mosaic_data <- table(df[[input$mosaic_x]], df[[input$mosaic_y]])
    mosaicplot(mosaic_data, main = paste("Mosaic Plot of", input$mosaic_x, "vs", input$mosaic_y), 
               color = TRUE, shade = TRUE, legend = TRUE)
  })
  
  # Download handler for mosaic plot with dynamic title
  output$downloadMosaic <- downloadHandler(
    filename = function() {
      paste("mosaicplot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      mosaic_data <- table(data()[[input$mosaic_x]], data()[[input$mosaic_y]])
      mosaicplot(mosaic_data, main = paste("Mosaic Plot of", input$mosaic_x, "vs", input$mosaic_y), 
                 color = TRUE, shade = TRUE, legend = TRUE)
      dev.off()
    }
  )
  
  # Boxplot output
  output$boxplot <- renderPlot({
    req(data(), input$boxplot_num, input$boxplot_cat)
    df <- data()
    ggplot(df, aes_string(x = input$boxplot_cat, y = input$boxplot_num)) +
      geom_boxplot(fill = "lightblue", color = "black") + theme_minimal() +
      labs(x = input$boxplot_cat, y = input$boxplot_num, 
           title = paste("Boxplot of", input$boxplot_num, "by", input$boxplot_cat))
  })
  
  # Download handler for boxplot with dynamic title
  output$downloadBox <- downloadHandler(
    filename = function() {
      paste("boxplot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      df <- data()
      p <- ggplot(df, aes_string(x = input$boxplot_cat, y = input$boxplot_num)) +
        geom_boxplot(fill = "lightblue", color = "black") + 
        theme_minimal() +
        labs(
          title = paste("Boxplot of", input$boxplot_num, "by", input$boxplot_cat),
          x = input$boxplot_cat,
          y = input$boxplot_num
        )
      print(p)
      dev.off()
    }
  )
  
  # Styled summary statistics table
  output$summary_table <- renderDT({
    df <- data()
    req(df)
    num_vars <- df[sapply(df, is.numeric)]
    summary_stats <- data.frame(
      Variable = names(num_vars),
      Mean = sapply(num_vars, mean, na.rm = TRUE),
      Median = sapply(num_vars, median, na.rm = TRUE),
      SD = sapply(num_vars, sd, na.rm = TRUE)
    )
    datatable(summary_stats, options = list(pageLength = 5), class = "summary-table")
  })
  
  # Display the first 10 rows of the uploaded data with enhanced features
  output$data_table <- renderDT({
    df <- data()
    req(df)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Create Shiny app
shinyApp(ui, server)
