library(shiny)

# Define UI for random distribution app ----
ui <- fluidPage(
    
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
            
            # Tabs ----
            tabsetPanel(type = "tabs",
                        # TODO: Implement different functionality separated by tabs ----
            )
            
        )
    )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
    
   
}
