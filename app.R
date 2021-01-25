#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DAAG)

data(biomass)

biomass[is.na(biomass)] <- 0

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Biomass Data Univariate statiscial analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("select", label = h3("Select feature"), 
                        choices = cbind(names(biomass)), 
                        selected = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("statsTableOut")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    tableStats <- reactive({
        if (!is.numeric(biomass[, input$select])) return(NULL)
        
        names.tmp <- c('Max', 'Min', 'Moyenne')
        summary.tmp <- c(max(biomass[, input$select]), min(biomass[, input$select]), mean(biomass[, input$select]))
        summary.tmp <- cbind.data.frame(names.tmp, summary.tmp)
        
        colnames(summary.tmp) <- c('Statistique', 'Valeur')
        
        summary.tmp
    });
    
    output$statsTableOut <- renderTable({ tableStats() })
}

# Run the application 
shinyApp(ui = ui, server = server)
