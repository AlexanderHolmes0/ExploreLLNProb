library(shiny)
library(tidyverse)
library(plotly)

ui <- fluidPage(

    
    titlePanel("Explore Coin Flip Probability"),

    
    sidebarLayout(
        sidebarPanel(
            sliderInput("flips",
                        "Number of flips:",
                        min = 1,
                        max = 1e06,
                        value = 1000,
                        step = 100,
                        animate = TRUE
                        )
        ,radioButtons(inputId = "type",
                      label = "Heads/Tails",
                      choices = c("Heads","Tails"),
                      selected = "Heads")),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot"),
           plotlyOutput("Hcounts"),
           plotlyOutput("Tcounts")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observed <- reactive({
        x <- data.frame(flip = 1:input$flips,
                   event=sample( c(1,0), prob=c(.5,.5), size=input$flips, replace=TRUE ))
        x$Tflag <- ifelse(x$event==0,1,0)
        print(x)
    })
    
    output$distPlot <- renderPlotly({
        total_observed <- cumsum(observed()$event)
        estimated_probability <- total_observed/1:input$flips
        
        ggplotly(ggplot(observed(),aes(flip, estimated_probability))+
            geom_line()+
            geom_hline(yintercept = 0.5,color='red')+
            labs(x="Number of Flips",y=paste0("Fraction of ",input$type))+
            ylim(0,1)+
            theme_bw())
        
    })
    
    output$Hcounts <- renderPlotly({
        
        total_observed <- cumsum(observed()$event)
        y <- total_observed - round((1:input$flips/2),0)
        
        ggplot(data.frame(),aes(1:as.numeric(input$flips), y))+
            geom_line()+
            geom_hline(yintercept = 0,color='red')+
            labs(x="Number of Flips",y=paste0("Count of Heads - Flips/2"))+
            theme_bw()
    })
    
    output$Tcounts <- renderPlotly({
        total_observed <- cumsum(observed()$Tflag)
        y <- total_observed - round((1:input$flips/2),0)
        
        ggplot(data.frame(),aes(1:as.numeric(input$flips), y))+
            geom_line()+
            geom_hline(yintercept = 0,color='red')+
            labs(x="Number of Flips",y=paste0("Count of Tails - Flips/2"))+
            theme_bw()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
