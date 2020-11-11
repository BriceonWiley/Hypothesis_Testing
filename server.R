# Cmd + Shift + Enter to run App

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    mu = as.numeric(input$mu)
    sig = as.numeric(input$sig)
    lb = mu - 4 * sig
    ub = mu + 4 * sig

    # draw the histogram with the specified number of bins
    ggplot() +
      stat_function(
        fun = dnorm, args = list(mean = mu, sd = sig)
      ) +
      lims(x = c(lb, ub))
  })
}
