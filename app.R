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

    fluidRow(
        column(6, selectInput("select", label = h3("Select feature"), 
                              choices = cbind(names(biomass)), 
                              selected = 1))
    ),
    
    fluidRow(
        conditionalPanel(
            condition="input.select != 'species' && input.select != 'fac26'",
            fluidRow(
                column(4, tableOutput("statsTableOut")),
                column(8, plotOutput("boxplotOut"))
            )
        ),
        column(12, plotOutput("batonsOut"))
    ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    tableStats <- reactive({
        if (!is.numeric(biomass[, input$select])) return(NULL)
        
        names.tmp <- c('Max', 'Min', 'Moyenne', '1e quartile', 'Médiane', '3e quartile', 'Variance', 'Écart-type')
        summary.tmp <- c(
            max(biomass[, input$select]), min(biomass[, input$select]), mean(biomass[, input$select]),
            quantile(biomass[, input$select])[2], median(biomass[, input$select]), quantile(biomass[, input$select])[4],
            var(biomass[, input$select]), sqrt(var(biomass[, input$select]))
            )
        summary.tmp <- cbind.data.frame(names.tmp, summary.tmp)
        
        colnames(summary.tmp) <- c('Statistique', 'Valeur')
        
        summary.tmp
    });
    
 
    output$statsTableOut <- renderTable({ tableStats() })
    output$boxplotOut <- renderPlot({
        if (!is.numeric(biomass[, input$select])) return(NULL)
        
        boxplot(biomass[, input$select], col = grey(0.8), main="Boîte à moustaches", ylab=input$select)
    })
    
    output$batonsOut <- renderPlot({
        plot(table(biomass[, input$select]), col ="purple4", xlab =input$select, ylab ="Effectifs", 
             main ="Distribution des effectifs pour la colonne")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
