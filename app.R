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
library(ggplot2)

data(biomass)

for (j in 1:6){
    biomass[, j][is.na(biomass[, j])] <- median(biomass[, j], na.rm=T)
}

# User Interface
ui <- fluidPage(
    
    # Application title
    titlePanel("Biomass Data Univariate/Bivariate statiscial analysis"),
    h4("Travail realisé par: Ilyes Kamel, Abdelkarim Azzaz et Achraf Louiza"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            tags$i("Toutes les variables sont quantitatives sauf 'species' et 'fac26' qui sont qualitatives"),
            selectInput("select", label = h4("Select a feature for univariate analysis"),
                        choices = names(biomass), 
                        selected = 1),
            selectInput("selectB", label = h4("Select another feature for bivariate analysis"),
                        choices = sort(names(biomass)), 
                        selected = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("summary", 
                         fluidRow(
                            column(7, align="center",
                                   # Zone d'affichage d'un résumé statistique
                                   tableOutput("statsTableOut")),
                            column(5,
                                   # Zone d'affichage d'un heatmap correlation
                                   plotOutput(outputId = "heatmapCorrelation"))
                        )),
                tabPanel("plots (univariate)",
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
                tabPanel("plots (bivariate)",
                         fluidRow(
                             column(6, fluidRow(
                                 column(12, plotOutput(outputId = "nuagePointsBiv")),
                                 column(4, offset = 3, textOutput("correlation"))
                             )),
                             column(6, 
                                    plotOutput(outputId = "histogrammeMod"))
                         ),
                         fluidRow(
                             column(6, 
                                    # Zone d'affichage de l'histogramme
                                    plotOutput(outputId = "effectifsCumCurveB")),
                             column(6, 
                                    # Zone d'affichage d'un summary
                                    plotOutput(outputId = "boxplotB"))
                         )),
                tabPanel("Table", dataTableOutput("table"), style = "font-size: 85%"),
                tabPanel("dataset info", includeMarkdown('dataset.Rmd'))
                         
            )
        )
    )
    
)

# Serveur
server <- function(input, output) {
    
    output$table <- renderDataTable({biomass})
    
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
        q = data.frame(statistiques = c('min', 'quantile 25%', 'median', 'quantile 75%',
                                        'max', 'moyenne', 'ecart type'),
                       values = c(quantile(biomass[, input$select]), 
                                  mean(biomass[, input$select]),
                                  sd(biomass[, input$select]))
                       )
        
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
        } else {
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

    ##Analyse bivariée: 
    output$nuagePointsBiv <- renderPlot({
        if (!is.numeric(biomass[, input$select]) & !is.numeric(biomass[, input$selectB])){
            return(ggplot(biomass, aes_string(x = input$select , fill=input$selectB)) + 
                       geom_bar() +
                       theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)))
        }
        if( is.numeric(biomass[, input$select]) &  is.numeric(biomass[, input$selectB])){
            plot(
                x = biomass[, input$select], y = biomass[, input$selectB],
                col = "red",
                main=paste(input$selectB, 'en fonction de ', input$select),
                xlab = input$select, ylab=input$selectB
            )
            
            abline(lm(biomass[, input$selectB]~biomass[, input$select]), col="blue", lwd = 2)
        }
        if(is.numeric(biomass[, input$select]) &  !is.numeric(biomass[, input$selectB])){
            return(ggplot(data=biomass)+
            geom_histogram(mapping = aes_string(input$select, fill=input$selectB), bins = 10)+
            xlab(label = input$select)+
            ylab(label="Frequency"))
        }
        if(!is.numeric(biomass[, input$select]) &  is.numeric(biomass[, input$selectB])){ 
            ggplot(data=biomass)+
                geom_histogram(mapping = aes_string(input$selectB, fill=input$select), bins = 10)+
                xlab(label = input$selectB)+
                ylab(label="Frequency")
        }
    })
    
    output$correlation <- renderText({
        if(! is.numeric(biomass[, input$select]) || ! is.numeric(biomass[, input$selectB])) return(NULL) 
        
        coeff_correlation.tmp <- cov(biomass[, input$select], biomass[, input$selectB])/(sqrt(var(biomass[, input$select])*var(biomass[, input$selectB])))
        paste('Coeff de corrélation linéaire = ', round(coeff_correlation.tmp, digits=2))
    })
    
    output$heatmapCorrelation <- renderPlot({
        nums.tmp <- unlist(lapply(biomass, is.numeric))
        corrMatrix.tmp = round(cor(biomass[, nums.tmp]), 2)
        
        heatmap(corrMatrix.tmp)
    })
    output$histogrammeMod = renderPlot({
        if( is.numeric(biomass[, input$select]) & is.numeric(biomass[, input$selectB])){  
        columns = c(input$select, input$selectB)
        # Reshape data()
        data.stack <- melt(biomass[, columns], measure.vars = columns)
        # Boxplot élaborée
        qplot(x = data.stack[,1], y = data.stack[,2], 
              xlab = "Modalités", ylab = "Mesures",
              geom=c("boxplot", "jitter"), fill=data.stack[,1]) +
            theme(legend.title=element_blank())
        }
    })

}

# Lancement de l'application 
shinyApp(ui = ui, server = server)


