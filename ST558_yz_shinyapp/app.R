library(shiny)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(caret)
library(mltools)
library(data.table)
library(parallel)
library(doParallel)
library(ggfortify)
library(shinydashboard)

seer18_sub <- read_rds("seer18_sub.rds")
seer18_sub <- na.omit(seer18_sub)
variables <-
    noquote(c("age_group", "Laterality", "Race_recode_W_B_AI_API", "Grade"))
use_cores <- detectCores() - 1

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
    sum_table <-
        cbind(Minimum, first_q, Median, Mean, SD, third_q, Max, samples)
    colnames(sum_table) <-
        c("Min.",
          "1st Qu.",
          "Median",
          "Mean",
          "SD",
          "3rd Qu.",
          "Max.",
          "N")
    return(sum_table)
}
encode_data <- function (df) {
    df[sapply(df, is.character)] <-
        lapply(df[sapply(df, is.character)], as.factor)
    recoded <- one_hot(data.table(df))
}

# Define dashboard UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Simple exploration, modeling and prediction using the NIH cancer case data sets",
                    titleWidth = 800),
    # Sidebar ----
    dashboardSidebar(
        #width = 3,
        h3("Data exploration", style = "font-size: 18px;"),
        # Select training/testing data split ratio
        selectInput(
            'data_year',
            'Year of data:',
            choices = unique(seer18_sub$Year_of_diagnosis),
            selected = "2005"
        ),
        selectInput(
            'selected_variable',
            'Variable for plotting:',
            choices = variables,
            selected = NULL
        ),
        checkboxInput(
            "sex",
            h4("Visualize by sex", style = "color:red;font-size: 15px;"),
            FALSE
        ),
        # Data modeling section supervised
        h3("Modeling parameters", style = "font-size: 18px;"),
        selectInput(
            'trainMethod',
            'Training method:',
            choices = c("mlr", "bagTree"),
            selected = "mlr"
        ),
        uiOutput("n_tree_slider"),
        uiOutput("cv_fold_slider"),
        uiOutput("cv_repeat_slider"),
        actionButton("train_model_sup", "Train Model"),
        # Data modeling section unsupervised
        h3("   "),
        actionButton("train_model_unsup", "PCA")
    ),
    
    # Main dashboard for displaying outputs ----
    dashboardBody(
        tabsetPanel(
            type = "tabs",
            id = "inTabset",
            tabPanel(
                "Data summary",
                tags$a(
                    href = "https://seer.cancer.gov/data/access.html",
                    "Click for Instructions on requesting and accessing the original data.",
                    style = "color:green;font-size:20px;"
                ),
                tags$hr(style = "border-color: grey;"),
                textOutput("summary_title"),
                tableOutput("table_summary"),
                h1("Sampled yearly data table", style = "font-size:20px;"),
                dataTableOutput("train_data_table"),
                downloadButton("download_full_table", "Download yearly table"),
                downloadButton("download_sampled_table", "Download train data table"),
                tags$head(tags$style("#summary_title{font-size: 20px;}"))
            ),
            tabPanel(
                "Visualization",
                fluidRow(
                    column(
                        7,
                        plotOutput('bar_plot') %>% withSpinner(color = "#0dc5c1"),
                        downloadButton("download_plot1", "Download plot")
                    ),
                    column(5,
                           plotOutput('hist_plot') %>% withSpinner(color = "#0dc5c1"))
                ),
                conditionalPanel(condition = "input.sex == 1",
                                 fluidRow(
                                     column(
                                         5,
                                         plotOutput('plot_by_sex', hover = "box_plot_click") %>% withSpinner(color = "#0dc5c1"),
                                         downloadButton("download_plot2", "Download plot")
                                     ),
                                     column(7,
                                            verbatimTextOutput("selected_rows"))
                                 ))
            ),
            tabPanel(
                "Modeling(Supervised)",
                verbatimTextOutput("mlr_sum") %>% withSpinner(color = "#0dc5c1"),
                verbatimTextOutput("bt_sum") %>% withSpinner(color = "#0dc5c1"),
                tags$hr(style = "border-color: grey;"),
                span(textOutput("pred_result"), style = "color:red;font-size: 15px;font-weight: bold;"),
                tags$hr(style = "border-color: grey;"),
                dataTableOutput("test_data_table"),
                downloadButton("download_test_table", "Download test data")
            ),
            tabPanel(
                "Modeling(Unsupervised)",
                verbatimTextOutput("pca_result") %>% withSpinner(color = "#0dc5c1"),
                plotOutput('pca_plot') %>% withSpinner(color = "#0dc5c1"),
                plotOutput('pc_plot') %>% withSpinner(color = "#0dc5c1")
            )
        )
    )
)






server <- function(input, output, session) {
    year_data <- reactive({
        seer18_sub %>% filter(Year_of_diagnosis == input$data_year)
    })
    # Data splitting into train and test sets
    train_index <- reactive({
        train_index <-
            createDataPartition(
                sample_train_data()$Survival_months,
                p = .7,
                list = FALSE,
                times = 1
            )
        train_index <- as.vector(train_index)
    })
    train_data <- reactive({
        train_data <- sample_train_data()[train_index(),]
    })
    test_data <- reactive({
        test_data <- sample_train_data()[-train_index(),]
    })
    # Output 5-number statistics for survived months
    selected_table <- reactive({
        summary_table <- desc_stats(year_data()$Survival_months)
    })
    
    # Output descriptive summary statistics
    output$summary_title <-
        renderText(paste0(
            "Descriptive summary statistics for months survived in ",
            input$data_year
        ))
    output$table_summary <- renderTable(selected_table())
    # Output DT for sample training data
    sample_train_data <- reactive({
        sample_n(year_data(), 50000)
    })
    output$train_data_table <-
        renderDataTable(
            sample_train_data(),
            class = "display nowrap compact",
            caption = "Table displayed shows only 50000 randomly sampled rows.",
            options = list(pageLength = 20),
            filter = "top"
        )
    # Download data tables
    output$download_full_table <- downloadHandler(
        filename = 'full_table.csv',
        content = function(file) {
            write.csv(year_data(), file, row.names = FALSE)
        }
    )
    output$download_sampled_table <- downloadHandler(
        filename = 'train_data_sampled_table.csv',
        content = function(file) {
            write.csv(train_data(), file, row.names = FALSE)
        }
    )
    # Render plots and allows downloading plots
    bar_plot_data <- reactive({
        if (input$sex == FALSE) {
            plot_vars <-
                year_data() %>% select(Survival_months, input$selected_variable)
            colnames(plot_vars) <- c("Survival_months", "Variable")
            to_plot <-
                plot_vars %>% group_by(Variable) %>% summarise(mean(Survival_months))
            to_plot$Variable <- as.factor(to_plot$Variable)
        } else {
            plot_vars <-
                year_data() %>% select(Survival_months,
                                       input$selected_variable,
                                       Sex_no_total)
            colnames(plot_vars) <-
                c("Survival_months", "Variable", "Sex")
            to_plot <-
                plot_vars %>% group_by(Variable, Sex) %>% summarise(mean(Survival_months))
            to_plot$Variable <- as.factor(to_plot$Variable)
        }
        to_plot
    })
    bar_plot_out <- reactive({
        if (input$sex == FALSE) {
            ggplot(data = bar_plot_data(), aes(x = `mean(Survival_months)`)) + 
                ylab("Months survived (Avg)") + ylab(input$selected_variable) + 
                geom_col(aes(y = Variable))  + theme(
                panel.background = element_rect(fill = 'transparent', color = 'black'),
                panel.grid = element_blank()
            )
        } else {
            ggplot(data = bar_plot_data(), aes(x = `mean(Survival_months)`)) + 
                ylab("Months survived (Avg)") + ylab(input$selected_variable) + 
                geom_col(aes(y = Variable, fill = Sex), position = "dodge") + theme(
                panel.background = element_rect(fill = 'transparent', color = 'black'),
                panel.grid = element_blank()
            )
        }
    })
    output$hist_plot <- renderPlot({
        plot(density(year_data()$Survival_months),
             xlab = "Months survived",
             main = "Density plot for the year")
    })
    output$bar_plot <- renderPlot({
        bar_plot_out()
    })
    # Interactive bar plot
    box_plot_data <- reactive({
        plot_vars <- year_data() %>% select(Survival_months, Sex_no_total)
        colnames(plot_vars) <- c("Survival_months", "Sex")
        plot_vars
    })
    box_plot <- reactive({
        p1 <-
            plot(
                as.factor(box_plot_data()$Sex),
                box_plot_data()$Survival_months,
                xlab = "Sex",
                ylab = "# months survived"
            )
    })
    output$selected_rows <- renderPrint({
        if (is.null(input$box_plot_click$x))
            return()
        else {
            keeprows <-
                round(input$box_plot_click$x) == as.numeric(as.factor(box_plot_data()$Sex))
            desc_stats(box_plot_data()[keeprows,]$Survival_months)
        }
    })
    output$download_plot1 <- downloadHandler(
        filename = 'by_variable.png',
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(
                    ...,
                    width = width,
                    height = height,
                    res = 300,
                    units = "in"
                )
            }
            ggsave(file, plot = bar_plot_out(), device = device)
        }
    )
    output$plot_by_sex <- renderPlot({
        box_plot()
    })
    
    output$download_plot2 <- downloadHandler(
        filename = 'by_sex.png',
        content = function(file) {
            png(file = file)
            box_plot()
            dev.off()
        },
        contentType = 'image/png'
    )
    observeEvent(input$sex, {
        updateTabsetPanel(session, "inTabset",
                          selected = "Visualization")
    })
    observeEvent(input$selected_variable, {
        updateTabsetPanel(session, "inTabset",
                          selected = "Visualization")
    })
    # Modeling pages
    # Jump to modeling tab if clicked
    observeEvent(input$train_model_sup, {
        updateTabsetPanel(session, "inTabset",
                          selected = "Modeling(Supervised)")
    })
    observeEvent(input$train_model_unsup, {
        updateTabsetPanel(session, "inTabset",
                          selected = "Modeling(Unsupervised)")
    })
    output$n_tree_slider <- renderUI({
        if (input$trainMethod == "bagTree") {
            sliderInput(
                'n_tree',
                "Adjust ntree values",
                min = 50,
                max = 500,
                step = 50,
                value = 100
            )
        }
    })
    output$cv_fold_slider <- renderUI({
        if (input$trainMethod == "bagTree") {
            sliderInput(
                'cv_fold',
                "Adjust # of CV folds",
                min = 5,
                max = 15,
                step = 1,
                value = 10
            )
        }
    })
    output$cv_repeat_slider <- renderUI({
        if (input$trainMethod == "bagTree") {
            sliderInput(
                'cv_repeat',
                "Adjust # of CV repeats",
                min = 1,
                max = 5,
                step = 1,
                value = 3
            )
        }
    })
    #Prep data for modeling
    recoded_data <- reactive({
        my_data <- train_data() %>% select(!Year_of_diagnosis)
        my_data <- encode_data(my_data)
        as.data.frame(my_data)
    })
    # MLR
    # modeling
    mlr_trained <- eventReactive(input$train_model_sup, {
        if (isolate(input$trainMethod) == "mlr") {
            linear_model_fit <- lm(Survival_months ~ ., isolate(recoded_data()))
            linear_model <-
                step((linear_model_fit),
                     direction = "both",
                     trace = 0)
        }
    })
    observeEvent(input$train_model_sup, {
        output$mlr_sum <- renderPrint({
            if (isolate(input$trainMethod) == "mlr") {
                summary(mlr_trained())
            }
        })
    })
    
    # bagged tree
    bt_trained <- eventReactive(input$train_model_sup, {
        if (isolate(input$trainMethod) == "bagTree") {
            cl <- makePSOCKcluster(use_cores)
            registerDoParallel(cl)
            bt_fit <-
                train(
                    Survival_months ~ .,
                    isolate(recoded_data()),
                    method = "gbm",
                    verbose = 0,
                    preProcess = c("center", "scale"),
                    trControl = trainControl(
                        method = "repeatedcv",
                        number = isolate(input$cv_fold),
                        repeats = isolate(input$cv_repeat),
                        allowParallel = TRUE,
                        verboseIter = FALSE
                    ),
                    tuneGrid = expand.grid(
                        n.trees = isolate(input$n_tree),
                        interaction.depth = c(1, 4, 9),
                        shrinkage = 0.1,
                        n.minobsinnode = 10
                    )
                )
            stopCluster(cl)
            bt_fit
        }
    })
    observeEvent(input$train_model_sup, {
        output$bt_sum <- renderPrint({
            if (isolate(input$trainMethod) == "bagTree") {
                bt_trained()
            }
        })
    })
    
    # Prediction
    output$test_data_table <-
        renderDataTable(
            test_data(),
            class = "display nowrap compact",
            caption = "Please select a row for prediction.",
            options = list(pageLength = 5, sDom  = '<"top">lrt<"bottom">ip'),
            selection = 'single',
            filter = "top"
        )
    output$download_test_table <- downloadHandler(
        filename = 'test_data.csv',
        content = function(file) {
            write.csv(test_data(), file, row.names = FALSE)
        }
    )
    selected_test_data <-
        eventReactive(input$test_data_table_rows_selected, {
            my_data <- test_data() %>% select(!Survival_months)
            my_data <-
                encode_data(my_data) %>% slice(input$test_data_table_rows_selected)
            as.data.frame(my_data)
        })
    observeEvent(input$test_data_table_rows_selected, {
        output$pred_result <- renderText({
            if (isolate(input$trainMethod) == "mlr") {
                months <- predict(mlr_trained(), newdata = selected_test_data())[1]
                paste0(
                    "The predicted number of months of survival based on selected case is: ",
                    round(months, digits = 0)
                )
            } else {
                months <- predict(bt_trained(), newdata = selected_test_data())[1]
                paste0(
                    "The predicted number of months of survival based on selected case is: ",
                    round(months, digits = 0)
                )
            }
        })
    })
    # Unsupervised learning
    pca_data <- reactive({
        my_data <- sample_n(seer18_sub, 50000)
        my_data <-
            my_data %>% select(
                Survival_months,
                Histologic_Type_ICDO3,
                Primary_Site,
                RX_SummSurg_Prim_Site_1998,
                age_group,
                Sex_no_total
            )
        my_data$Sex_no_total <- as.factor(my_data$Sex_no_total)
        my_data
    })
    pca_out <- eventReactive(input$train_model_unsup, {
        prcomp(select(pca_data(),!Sex_no_total),
               center = TRUE,
               scale. = TRUE)
    })
    observeEvent(input$train_model_unsup, {
        output$pca_result <- renderPrint(summary(pca_out()))
    })
    observeEvent(input$train_model_unsup, {
        output$pca_plot <- renderPlot(
            plot(
                cumsum(pca_out()$sdev ^ 2 / sum(pca_out()$sdev ^ 2)),
                xlab = "PCs",
                ylab = "Cumulative Proportion of Variance Explained",
                ylim = c(0, 1),
                type = 'b'
            )
        )
    })
    observeEvent(input$train_model_unsup, {
        output$pc_plot <- renderPlot(
            autoplot(
                pca_out(),
                data = pca_data(),
                colour = 'Sex_no_total',
                loadings = TRUE,
                loadings.colour = 'black',
                loadings.label = TRUE,
                loadings.label.size = 4) + theme(
                panel.background = element_rect(fill = 'transparent', color = 'black'),
                panel.grid = element_blank()
            )
        )
    })
}



shinyApp(ui, server)
