library(shiny)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(caret)

seer18_sub <- read_rds("seer18_sub.rds")
seer18_sub <- na.omit(seer18_sub)

# Define UI for random distribution app ----
ui <- navbarPage("Cancer Survival",
    tabPanel("Data summary",
        fluidPage(
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(width = 3,
                    selectInput('data_ratio', 'Please select training/testing data ratio(%):', choices = c("90/10", "80/20", "70/30", "60/40"), selected = "80/20"),
                    # TODO: Data loading, importing, selection ----
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(width = 9,
                    tabsetPanel(type = "tabs",
                        tabPanel("Training Data Table",
                                 dataTableOutput("train_data_table"),
                                 downloadButton("download_full_table", "Download full table"),
                                 downloadButton("download_sampled_table", "Download sample table")
                                 )
#                        tabPanel("Plots")
                    )
                )
            )
        )
    ),
    tabPanel("Data modeling",
        fluidPage(
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
                 
                # Sidebar panel for inputs ----
                sidebarPanel(
                # TODO: Data loading, importing, selection ----
                ),
                 
                # Main panel for displaying outputs ----
                mainPanel(
                     
                )
            )
        )
    )
)

# Define server logic for random distribution app ----
server <- function(input, output, session) {
    # Data splitting into train and test sets
    train_index <- reactive({
        if (input$data_ratio == "90/10") {
            prob <- .9
        } else if (input$data_ratio == "80/20") {
            prob <- .8
        } else if (input$data_ratio == "70/30") {
            prob <- .7
        } else {
            prob <- .6
        }
        train_index <- createDataPartition(seer18_sub$Survival_months,
                                           p = prob,
                                           list = FALSE,
                                           times = 1)
        train_index <- as.vector(train_index)
    })
    train_data <- reactive({
        train_data <- seer18_sub[train_index(), ]
    })
    test_data <- reactive({
        test_data <- seer18_sub[-train_index(), ]
    })
    # Output DT for sample training data
    sample_train_data <- reactive({
        sample_n(train_data(), 100000)
    })
    output$train_data_table <-
        renderDataTable(sample_train_data(), class = "display nowrap compact", caption="Table displayed shows only 100000 randomly sampled rows. Please note that the full table is ~270MB.", options = list(pageLength = 20), filter = "top")
    # Download data tables
    output$download_full_table <- downloadHandler(
        filename = 'train_data_full_table.csv',
        content = function(file) {
            write.csv(train_data(), file, row.names = FALSE)
        }
    )
    output$download_sampled_table <- downloadHandler(
        filename = 'train_data_sampled_table.csv',
        content = function(file) {
            write.csv(sample_train_data(), file, row.names = FALSE)
        }
    )
}



shinyApp(ui, server)
