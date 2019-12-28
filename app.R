#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readabs)
library(tidyverse)

# Read data pre-downloaded
cat_nos <- c(LFS = "6202.0", National.Accounts = "5206.0")

abs_data <- map_dfr(cat_nos, ~ read_abs(., path = "data"))

# Define UI for app
ui <- fluidPage(
    
    # Application title
    titlePanel("ABS Plotter"),
    
    # Sidebar with inputs for selecting the series to plot 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "catNumber",
                        label = "Select Catalogue Number:",
                        choices = cat_nos,
                        multiple = FALSE),
            uiOutput("tableDropdown"),
            uiOutput("seriesDropdown"),
            uiOutput("seriesTypeDropdown")
        ),
        
        # Show a plot of the series
        mainPanel(
            plotOutput("absPlot")
        )
    )
)


# Define server logic 
server <- function(input, output) {

    plot_data <- reactive({
        # Remove decimals in catalogue number
        cat_no <- str_remove(input$catNumber, ".")
        # Filter to the selected catalogue number
        abs_data %>% 
            filter(str_detect(table_no, cat_no))
    })
    
    # Reactive UI element: gives tables for the selected catalogue number
    output$tableDropdown <- renderUI({
        tables <- plot_data() %>% 
            pull(table_title) %>% 
            unique()
        selectInput("tableNumber",
                    label = "Select Table Number:",
                    choices = tables)
    })
    
    # Reactive UI element: gives series for the selected catalogue number
    # and table combination
    output$seriesDropdown <- renderUI({
        series <- plot_data() %>% 
            filter(table_title == input$tableNumber) %>% 
            pull(series) %>% 
            unique()
        selectInput("series",
                    label = "Select Series:",
                    choices = series)
    })
    
    # Reactive UI element: gives series type (e.g. trend, seasonally adjusted,
    # etc.) for the given cat no, table, and series combination
    output$seriesTypeDropdown <- renderUI({
        series_type <- plot_data() %>% 
            filter(table_title == input$tableNumber,
                   series == input$series) %>% 
            pull(series_type) %>% 
            unique()
        selectInput("seriesType",
                    label = "Select Series Type:",
                    choices = series_type)
    })
    
    # Plot series
    output$absPlot <- renderPlot({
        plot_data() %>% 
            filter(table_title == input$tableNumber,
                   series == input$series,
                   series_type == input$seriesType) %>% 
            ggplot(aes(x = date, y = value)) +
            geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
