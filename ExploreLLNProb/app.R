library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$head(
    HTML(
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
<meta name="twitter:image" content="https://media0.giphy.com/media/xUn3CftPBajoflzROU/giphy.gif?cid=ecf05e4774lrky547c5mpouz1upc5tk13kre44jkok0tqr5u&ep=v1_gifs_search&rid=giphy.gif&ct=g">'
    ),
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$style(HTML("

      #bottomright{
        float: right;
        bottom: 1vh;
      }
      #user-select-none svg-container{
        height: 250px;
      }

      "))
  ),
  titlePanel("Explore Probability Quirks"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("sliders")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = "example",
        tabPanel(
            "Coin Flips Example",
            value = 1,
            plotlyOutput("distPlot"),
            tabsetPanel(
              tabPanel("Counts - n/2", plotlyOutput("Hcounts",height='350px')),
              tabPanel("Actual Count Difference", plotlyOutput("Tcounts",height='350px'))
            )
        ),
        tabPanel(
          "Dice Example",
          plotlyOutput("dicePlot"),
          tabsetPanel(
            tabPanel("Counts - n/6", plotlyOutput("diceCounts",height='350px')),
            tabPanel("Count Differences", plotlyOutput("diceCountsReal",height='350px'))
          )
        )
      ),
      div(
        id = "bottomright",
        tags$a(target = "_blank", rel = "noopener noreferrer", href = "https://github.com/AlexanderHolmes0", "Created by Alex Holmes")
      )
    )
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  output$sliders <- renderUI({
    if (input$example == 1) {
      list(sliderInput("trials",
        "Number of trials:",
        min = 1,
        max = 1e06,
        value = 1000,
        step = 1000,
        animate = TRUE,
        ticks = FALSE
      ))
    } else {
      list(
        sliderInput("rolls",
          "Number of Rolls:",
          min = 1,
          max = 1e06,
          value = 1000,
          step = 1000,
          animate = TRUE,
          ticks = FALSE
        ),
        selectInput(
          inputId = "dicenum",
          label = "Dice Number",
          choices = c("ones", "twos", "threes", "fours", "fives", "sixes"),
          selected = "ones",
          multiple = TRUE
        )
      )
    }
  })


  observed_coin <- reactive({
    x <- data.frame(
      flip = 1:input$trials,
      event = sample(c(1, 0), prob = c(.5, .5), size = input$trials, replace = TRUE)
    )
    x
  })

  output$distPlot <- renderPlotly({
    req(input$trials)
    total_observed <- cumsum(observed_coin()$event)
    estimated_probability <- total_observed / 1:input$trials

    total_observedT <- cumsum(observed_coin()$event == 0)
    estimated_probabilityT <- total_observedT / 1:input$trials

    ggplotly(ggplot(isolate(observed_coin()), aes(flip, estimated_probability, color = "Heads")) +
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
    req(input$trials)
    total_observed <- cumsum(observed_coin()$event)
    y <- total_observed - (1:input$trials / 2)

    total_observedT <- cumsum(observed_coin()$event == 0)
    yT <- total_observedT - (1:input$trials / 2)

    ggplotly(ggplot(isolate(observed_coin()), aes(flip, y, color = "Heads")) +
      geom_line() +
      geom_line(aes(flip, yT, color = "Tails")) +
      geom_hline(yintercept = 0, color = "red", alpha = 0.5) +
      labs(title = "Number of Heads and Tails Don't Cancel Out", x = "Number of Flips", y = "Cumulative Counts - n/2") +
      theme_bw() +
      scale_color_manual("Event", values = c("Heads" = "#FF7276", "Tails" = "lightblue")) +
      scale_x_log10())
  })

  output$Tcounts <- renderPlotly({
    req(input$trials)
    `Heads-Tails` <- cumsum(observed_coin()$event) - cumsum(observed_coin()$event == 0)
    `Tails-Heads` <- cumsum(observed_coin()$event == 0) - cumsum(observed_coin()$event)

    ggplotly(ggplot(isolate(observed_coin()), aes(flip, `Heads-Tails`, color = "Heads-Tails")) +
      geom_line() +
      geom_line(aes(flip, `Tails-Heads`, color = "Tails-Heads")) +
      geom_hline(yintercept = 0, color = "red", alpha = 0.5) +
      labs(title = "Number of Heads and Tails Don't Cancel Out", x = "Number of Flips", y = "Cumulative Counts Difference") +
      theme_bw() +
      scale_color_manual("Event", values = c("Heads-Tails" = "#FF7276", "Tails-Heads" = "lightblue")) +
      scale_x_log10())
  })



  #-----------------------------------------------------------------------------

  observed_dice <- reactive({
    rolls <- data.frame(
      roll_num = 1:input$rolls,
      event = sample(1:6, size = input$rolls, replace = TRUE)
    )
    rolls$ones <- cumsum(rolls$event == 1)
    rolls$twos <- cumsum(rolls$event == 2)
    rolls$threes <- cumsum(rolls$event == 3)
    rolls$fours <- cumsum(rolls$event == 4)
    rolls$fives <- cumsum(rolls$event == 5)
    rolls$sixes <- cumsum(rolls$event == 6)
    rolls |>
      pivot_longer(3:8, values_to = "count") |>
      mutate(
        name = factor(name, ordered = TRUE, levels = c("ones", "twos", "threes", "fours", "fives", "sixes")),
        estimated_probability = count / roll_num
      )
  })

  output$dicePlot <- renderPlotly({
    req(input$rolls)
    req(input$dicenum)

    ggplotly(isolate(observed_dice()) |>
      filter(name %in% input$dicenum) |>
      ggplot(aes(roll_num, estimated_probability, color = name)) +
      geom_line() +
      geom_hline(yintercept = 1 / 6, color = "red", alpha = 0.5) +
      labs(title = "Fraction by Dice Number", x = "Number of Rolls (Log10)", y = paste0("Fraction")) +
      ylim(0, 1) +
      theme_bw() +
      scale_x_log10())
  })

  output$diceCounts <- renderPlotly({
    req(input$rolls)
    req(input$dicenum)

    ggplotly(isolate(observed_dice()) |>
      filter(name %in% input$dicenum) |>
      mutate(y = count - (roll_num / 6)) |>
      ggplot(aes(roll_num, y, color = name)) +
      geom_line() +
      geom_hline(yintercept = 1 / 6, color = "red", alpha = 0.5) +
      labs(title = "Cumulative Counts - n/6", x = "Number of Rolls (Log10)", y = paste0("Cumulative Counts - n/6")) +
      theme_bw() +
      scale_x_log10())
  })

  output$diceCountsReal <- renderPlotly({
    req(input$rolls)
    req(input$dicenum)

    ggplotly(isolate(observed_dice()) |>
      filter(name %in% input$dicenum) |>
      ggplot(aes((1 / 6)-estimated_probability )) +
      geom_histogram(binwidth = .0001) +
      labs(x = "Count - n/6"))
    # mutate(y = count - roll_num) |>
    # ggplot(aes(roll_num, y, color = name)) +
    # geom_line() +
    # geom_hline(yintercept = 1 / 6, color = "red", alpha = 0.5) +
    # labs(title = "Cumulative Counts - n", x = "Number of Rolls (Log10)", y = paste0("Fraction")) +
    # theme_bw() +
    # scale_x_log10())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
