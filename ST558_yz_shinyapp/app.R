library(shiny)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(caret)

seer18_sub <- read_rds("seer18_sub.rds")
seer18_sub <- na.omit(seer18_sub)
variables <- noquote(c("Year_of_diagnosis", "Age_recode_with_1_year_olds", "Laterality", "Race_recode_W_B_AI_API", "Grade"))

# Functions
desc_stats <- function (Var1) {
    Minimum <- min(Var1)
    Median <- median(Var1)
    SD <- sd(Var1)
    Mean <- mean(Var1)
    Max <- max(Var1)
    first_q <- quantile(Var1)[[2]]
    third_q <- quantile(Var1)[[4]]
    samples <- length(Var1)
    sum_table <- cbind(Minimum, first_q, Median, Mean, SD, third_q, Max, samples)
    colnames(sum_table) <- c("Min.", "1st Qu.", "Median", "Mean", "SD", "3rd Qu.", "Max.", "N")
    return(sum_table)
}

# Define UI for random distribution app ----
ui <- fluidPage(
            # Sidebar layout with input and output definitions ----
            sidebarLayout(
                # Sidebar panel for inputs ----
                sidebarPanel(width = 3,
                    h3("Data exploration", style = "font-size: 18px;"),
                    # Select training/testing data split ratio
                    selectInput('data_ratio', 'Training/testing data ratio(%):', choices = c("90/10", "80/20", "70/30", "60/40"), selected = "80/20"),
                    # 
                    selectInput('desc_table', 'Descriptive Summary Stat for:', choices = c("Full data", "Train data", "Test data"), selected = "Full data"),
                    selectInput('selected_variable', 'Variable for plotting:', choices = variables, selected = NULL),
                    checkboxInput("sex", h4("Visualize by sex", style = "color:red;font-size: 15px;"), FALSE),
                    h3("Modeling parameters", style = "font-size: 18px;"),
                    h3("Predictions", style = "font-size: 18px;")
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(width = 9,
                    tabsetPanel(type = "tabs",
                        tabPanel("Data summary",
                                 textOutput("summary_title"),
                                 tableOutput("table_summary"),
                                 h1("Sampled training data table", style = "font-size:20px;"),
                                 dataTableOutput("train_data_table"),
                                 downloadButton("download_full_table", "Download full table"),
                                 downloadButton("download_sampled_table", "Download sample table"),
                                 tags$head(tags$style("#summary_title{font-size: 20px;}"))),
                        tabPanel("Visualization",
                                plotOutput('bar_plot') %>% withSpinner(color = "#0dc5c1"),
                                conditionalPanel("input.sex == 1",
                                    plotOutput('plot_by_sex') %>% withSpinner(color = "#0dc5c1")
                                )
                        ),
                        tabPanel("Modeling",
                                
                        )
                    )
                )
            )
        )










# Define server logic for random distribution app ----
server <- function(input, output, session) {
    # Data splitting into train and test sets
    train_index <- eventReactive(input$data_ratio, {
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
    # Output 5-number statistics for survived months
    selected_data <- reactive({
        if (input$desc_table == "Full data") {
            data <- seer18_sub
        } else if (input$desc_table == "Train data") {
            data <- train_data()
        } else {
            data <- test_data()
        }
        data
    })
    selected_table <- reactive({
        summary_table <- desc_stats(selected_data()$Survival_months)
    })
    
    # Output descriptive summary statistics
    output$summary_title <- renderText(paste0("Descriptive summary statistics for ", input$desc_table))
    output$table_summary <- renderTable(selected_table())
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
    # Render barplots
    bar_plot_data <- reactive({
        if (input$sex == FALSE){
            plot_vars <- selected_data() %>% select(Survival_months, input$selected_variable) 
            colnames(plot_vars) <- c("Survival_months", "Variable")
            to_plot <- plot_vars %>% group_by(Variable) %>% summarise(mean(Survival_months))
            p1 <- ggplot(data = to_plot, aes(x = `mean(Survival_months)`)) + ylab("Months survived (Avg)") +
                ylab(input$selected_variable) + geom_col(aes( y = Variable))
        } else {
            plot_vars <- selected_data() %>% select(Survival_months, input$selected_variable, Sex_no_total) 
            colnames(plot_vars) <- c("Survival_months", "Variable", "Sex")
            to_plot <- plot_vars %>% group_by(Variable, Sex) %>% summarise(mean(Survival_months))
            p1 <- ggplot(data = to_plot, aes(x = `mean(Survival_months)`)) + ylab("Months survived (Avg)") +
                ylab(input$selected_variable) + geom_col(aes( y = Variable, fill = Sex), position="dodge")
        }
        p1
    })
    output$bar_plot <- renderPlot({
        bar_plot_data()
    })
    box_plot_data <- reactive({
        plot_vars <- selected_data() %>% select(Survival_months, Sex_no_total) 
        colnames(plot_vars) <- c("Survival_months", "Sex")
        p1 <- ggplot(data = plot_vars) + ylab("Months survived") +
            xlab("Sex") + geom_boxplot(aes(x = Sex, y = Survival_months))
        p1
    })
    output$plot_by_sex <- renderPlot({
        box_plot_data()
    })
}



shinyApp(ui, server)
