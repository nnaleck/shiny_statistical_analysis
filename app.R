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

# User Interface
ui <- fluidPage(
    
    # Application title
    titlePanel("Biomass Data Univariate statiscial analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("select", label = h3("Select feature"), 
                        choices = names(biomass), 
                        selected = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("summary", tableOutput("statsTableOut")),
                tabPanel("plots",
                         fluidRow(
                             column(6, 
                                    # Zone d'affichage de l'histogramme
                                    plotOutput(outputId = "effectifsHist")),
                             column(6, 
                                    # Zone d'affichage d'un summary
                                    plotOutput(outputId = "frequenceHist"))
                         ),
                         fluidRow(
                             column(6, 
                                    # Zone d'affichage de l'histogramme
                                    plotOutput(outputId = "effectifsCumCurve")),
                             column(6, 
                                    # Zone d'affichage d'un summary
                                    plotOutput(outputId = "boxplot"))
                         )),
                tabPanel("dataset info", includeMarkdown('dataset.Rmd'))
                         
            )
        )
    )
    
)

# Serveur
server <- function(input, output) {
    # Tableau statistique[qualitative]
    tabStatsQual <- reactive({
        # Calculer les effectifs et les effectifs cumules
        table.tmp <- as.data.frame(table(biomass[, input$select]))
        table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
        # Calculer les fréquences et les fréquences cumulés
        table.tmp <- cbind(table.tmp, 
                           table.tmp[[2]]/nrow(biomass)*100,
                           table.tmp[[3]]/nrow(biomass)*100)
        # Ajouter des noms de colonnes
        colnames(table.tmp) <- c(input$select, "Effectifs", "Effectifs Cum.",
                                 "Fréquences", "Fréquences Cum.")
        # Renvoyer le tableau statistique
        table.tmp
    })
    
    # Tableau statistique [quantitative]
    tabStatsQuant <- reactive({
        names.tmp <- c('Max', 'Min', 'Moyenne')
        summary.tmp <- c(max(biomass[, input$select]), min(biomass[, input$select]), mean(biomass[, input$select]))
        summary.tmp <- cbind.data.frame(names.tmp, summary.tmp)
        
        colnames(summary.tmp) <- c('Statistique', 'Valeur')
        
        summary.tmp
    });
    
    output$statsTableOut <- renderTable({ 
        if (is.numeric(biomass[, input$select]))
        {
            tabStatsQuant()
        } else {
            tabStatsQual() 
        }
    })
    
    # Histogramme des effectifs
    output$effectifsHist <- renderPlot({
        if (is.numeric(biomass[, input$select])){
            hist( biomass[, input$select], freq = TRUE, col = "blue",
                  main = paste("Histogramme de", input$select, sep=" "),
                  xlab = paste("Indice de", input$select, sep=" "), ylab = "Effectifs")
        }else{
            # Calcul des effectifs
            effectifs <- table(biomass[, input$select])
            # Diagramme en colonnes
            barplot(effectifs, ylab="Effectifs",
                        main = paste("diagramme en baton de ", input$select, sep=""))
                
        }
         
    })
    
    # Boxplot
    output$boxplot <- renderPlot({
        if(! is.numeric(biomass[, input$select])) return(NULL)
        
        boxplot(biomass[, input$select], main=paste("Boxplot de", input$select, sep=" "))
    })
    
    #Courbe cumulative
    output$effectifsCumCurve <- renderPlot({
        if(! is.numeric(biomass[, input$select])) return(NULL)
     
        #Recuperation des donnees a partir de l'histogramme
        tmp.hist <- hist( biomass[, input$select], plot = FALSE,
                        right = FALSE)
    
        plot(x = tmp.hist$breaks[-1], y = cumsum(tmp.hist$counts),
            xlab = input$select,
            ylab = "Effectifs cumules",
            main = paste("Courbe cumulative de ", input$select, sep=""),
            type = "o", col = "blue", lwd = 2)
    })
    
    #Histogramme(frequencies)
    output$frequenceHist <- renderPlot({
        if (is.numeric(biomass[, input$select]))
        {
             hist( biomass[, input$select], freq = FALSE,
                main = paste("Histogramme de ", input$select, sep=""), col = "green",
                xlab = input$select, ylab = "Densite de frequences", 
                right = FALSE,)
        } else {
            # Calcul des effectifs
            effectifs <- table(biomass[, input$select])
            #Diagramme en secteur
            pie(effectifs, 
                main =paste("diagramme en secteurs de ", input$select, sep=""))
        }
    })
    
    # Informations sur le dataset
    output$datasetInfo <- renderPrint(help(biomass))
}

# Lancement de l'application 
shinyApp(ui = ui, server = server)


