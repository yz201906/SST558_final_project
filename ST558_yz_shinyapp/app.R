library(shiny)
library(readr)

seer18_sub <- read_rds("seer18_sub.rds")

# Define UI for random distribution app ----
ui <- navbarPage("Cancer Survival",
    tabPanel("Data summary",
        fluidPage(
            # App title ----
            titlePanel("TBD"),
            
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
    ),
    tabPanel("Data modeling",
             fluidPage(
                 # App title ----
                 titlePanel("TBD"),
                 
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
server <- function(input, output) {
    
   
}
