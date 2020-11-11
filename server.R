# Cmd + Shift + Enter to run App

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({
    plot_type <- input$test

# Normal Distribution -----------------------------------------------------

    if (plot_type == "Normal") {
      mu <- as.numeric(input$mu)
      sigma <- as.numeric(input$sig)
      lb <- mu - 4 * sigma
      ub <- mu + 4 * sigma
      ggplot() +
        stat_function(fun = dnorm, args = list(mean = mu, sd = sigma)) +
        lims(x = c(lb, ub))


# t Distribution ----------------------------------------------------------

    } else if (plot_type == "t") {
      df <- as.numeric(input$dft)
      ggplot() +
        stat_function(fun = dt, args = list(df = df)) +
        lims(x = c(-4, 4))


# Chi Square Distribution -------------------------------------------------

    } else if (plot_type == "Chi Square") {
      df <- as.numeric(input$dfc)
      ggplot() +
        stat_function(fun = dchisq, args = list(df = df)) +
        lims(x = c(0, 10))


# F Distribution ----------------------------------------------------------

    } else if (plot_type == "F") {
      df1 <- as.numeric(input$df1)
      df2 <- as.numeric(input$df2)

      ggplot() +
        stat_function(fun = df, args = list(df1 = df1, df2 = df2)) +
        lims(x = c(0, 10))
    }
  })
}
