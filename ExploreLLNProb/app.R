library(shiny)
library(tidyverse)
library(plotly)

ui <- fluidPage(
    tags$head(HTML(
'<!-- Quick & Dirty HTML Meta Tags -->
<title>Explore Probability Quirks</title>
<meta name="description" content="Mess Around With Probability Simulations">

<!-- Google / Search Engine Tags -->
<meta itemprop="name" content="Explore Probability Quirks">
<meta itemprop="description" content="Mess Around With Probability Simulations">
<meta itemprop="image" content="https://media0.giphy.com/media/xUn3CftPBajoflzROU/giphy.gif?cid=ecf05e4774lrky547c5mpouz1upc5tk13kre44jkok0tqr5u&ep=v1_gifs_search&rid=giphy.gif&ct=g">

<!-- Google / Search Engine Tags -->
<meta name="title" content="Explore Probability Quirks">
<meta name="description" content="Mess Around With Probability Simulations">
<meta name="image" content="https://media0.giphy.com/media/xUn3CftPBajoflzROU/giphy.gif?cid=ecf05e4774lrky547c5mpouz1upc5tk13kre44jkok0tqr5u&ep=v1_gifs_search&rid=giphy.gif&ct=g">

<!-- Facebook Meta Tags -->
<meta property="og:url" content="https://aholmes25.shinyapps.io/ExploreLLNProb/">
<meta property="og:type" content="website">
<meta property="og:title" content="Explore Probability Quirks">
<meta property="og:description" content="Mess Around With Probability Simulations">
<meta property="og:image" content="https://media0.giphy.com/media/xUn3CftPBajoflzROU/giphy.gif?cid=ecf05e4774lrky547c5mpouz1upc5tk13kre44jkok0tqr5u&ep=v1_gifs_search&rid=giphy.gif&ct=g">

<!-- Twitter Meta Tags -->
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:url" content="https://aholmes25.shinyapps.io/ExploreLLNProb/">
<meta name="twitter:title" content="Explore Probability Quirks">
<meta name="twitter:description" content="Mess Around With Probability Simulations">
<meta name="twitter:image" content="https://media0.giphy.com/media/xUn3CftPBajoflzROU/giphy.gif?cid=ecf05e4774lrky547c5mpouz1upc5tk13kre44jkok0tqr5u&ep=v1_gifs_search&rid=giphy.gif&ct=g">')),
  titlePanel("Explore Probability Quirks"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("flips",
        "Number of flips:",
        min = 1,
        max = 1e06,
        value = 1000,
        step = 1000,
        animate = TRUE,
        ticks = FALSE
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      hr(),
      tabsetPanel(
        tabPanel("Counts - n/2", plotlyOutput("Hcounts")),
        tabPanel(
          "Actual Count Difference",
          plotlyOutput("Tcounts")
        )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  observed <- reactive({
    x <- data.frame(
      flip = 1:input$flips,
      event = sample(c(1, 0), prob = c(.5, .5), size = input$flips, replace = TRUE)
    )
    x$Tflag <- ifelse(x$event == 0, 1, 0)
    x
  })

  output$distPlot <- renderPlotly({
    total_observed <- cumsum(observed()$event)
    estimated_probability <- total_observed / 1:input$flips

    total_observedT <- cumsum(observed()$Tflag)
    estimated_probabilityT <- total_observedT / 1:input$flips

    ggplotly(ggplot(observed(), aes(flip, estimated_probability, color = "Heads")) +
      geom_line() +
      geom_line(aes(flip, estimated_probabilityT, color = "Tails")) +
      geom_hline(yintercept = 0.5, color = "red", alpha = 0.5) +
      labs(title = "Fraction of Heads/Tails", x = "Number of Flips (Log10)", y = paste0("Fraction")) +
      ylim(0, 1) +
      theme_bw() +
      scale_color_manual("Event", values = c("Heads" = "#FF7276", "Tails" = "lightblue")) +
      scale_x_log10())
  })

  output$Hcounts <- renderPlotly({
    total_observed <- cumsum(observed()$event)
    y <- total_observed - round((1:input$flips / 2), 0)

    total_observedT <- cumsum(observed()$Tflag)
    yT <- total_observedT - round((1:input$flips / 2), 0)

    ggplotly(ggplot(observed(), aes(flip, y, color = "Heads")) +
      geom_line() +
      geom_line(aes(flip, yT, color = "Tails")) +
      geom_hline(yintercept = 0, color = "red") +
      labs(title = "Number of Heads and Tails Don't Cancel Out", x = "Number of Flips", y = "Cumulative Counts - n/2") +
      theme_bw() +
      scale_color_manual("Event", values = c("Heads" = "#FF7276", "Tails" = "lightblue")))
  })

  output$Tcounts <- renderPlotly({
    `Heads-Tails` <- cumsum(observed()$event) - cumsum(observed()$Tflag)
    `Tails-Heads` <- cumsum(observed()$Tflag) - cumsum(observed()$event)

    ggplotly(ggplot(observed(), aes(flip, `Heads-Tails`, color = "Heads-Tails")) +
      geom_line() +
      geom_line(aes(flip, `Tails-Heads`, color = "Tails-Heads")) +
      geom_hline(yintercept = 0, color = "red") +
      labs(title = "Number of Heads and Tails Don't Cancel Out", x = "Number of Flips", y = "Cumulative Counts") +
      theme_bw() +
      scale_color_manual("Event", values = c("Heads-Tails" = "#FF7276", "Tails-Heads" = "lightblue")))
  })

  # output$Tcounts <- renderPlotly({
  #     total_observed <- cumsum(observed()$Tflag)
  #     y <- total_observed - round((1:input$flips/2),0)
  #
  #     ggplot(data.frame(),aes(1:as.numeric(input$flips), y))+
  #         geom_line()+
  #         geom_hline(yintercept = 0,color='red')+
  #         labs(x="Number of Flips",y=paste0("Count of Tails - Flips/2"))+
  #         theme_bw()
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
