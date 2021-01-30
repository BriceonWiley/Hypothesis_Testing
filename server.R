# Cmd + Shift + Enter to run App

server <- function(input, output) {


# Plots -------------------------------------------------------------------

  output$distPlot <- renderPlot({
    test_type <- input$test


# One Sample Proportion ---------------------------------------------------
    if (test_type == "One Sample Proportion") {
      # necessary set up
      p <- input$p
      n <- input$n
      sigma <- sqrt(p * (1 - p) / n)
      phat <- input$phat
      alpha <- input$alpha
      lb <- p - 4 * sigma
      ub <- p + 4 * sigma
      hypothesis_type <- input$hypot

      # base plot
      one_samp_prop <- ggplot() +
        stat_function(fun = dnorm, args = list(mean = p, sd = sigma)) +
        geom_point(aes(x = phat, y = 0.25), shape = 6, size = 7) +
        geom_point(aes(x = p, y = -0.25), shape = 17, size = 7) +
        lims(x = c(lb, ub)) +
        theme_minimal() +
        labs(x = "Sample Proporiton", y = "Probability")

      if (hypothesis_type == '<=') {
        # less than or equal to
        one_samp_prop +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(lb, qnorm(alpha, p, sigma)),
            alpha = 0.75, fill = "brown"
          ) +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(lb, phat),
            alpha = 0.5, fill = "grey"
          )
      } else if (hypothesis_type == '>=') {
        # greater than or equal to
        one_samp_prop +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(qnorm(1 - alpha, p, sigma), ub),
            alpha = 0.75, fill = "brown"
          ) +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(phat, ub),
            alpha = 0.5, fill = "grey"
          )
      } else if (hypothesis_type == '!=') {
        # not equal to
        one_samp_prop +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(lb, qnorm(alpha / 2, p, sigma)),
            alpha = 0.75, fill = "brown"
          ) +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(qnorm(1 - alpha / 2, p, sigma), ub),
            alpha = 0.75, fill = "brown"
          ) + {
            if (phat >= p) {
              stat_function(
                fun = dnorm, args = list(mean = p, sd = sigma),
                geom = "area", xlim = c(phat, ub),
                alpha = 0.5, fill = "grey"
              )
            } else {
              stat_function(
                fun = dnorm, args = list(mean = p, sd = sigma),
                geom = "area", xlim = c(lb, phat),
                alpha = 0.5, fill = "grey"
              )
            }
          }
      }
    }
# # Normal Distribution -----------------------------------------------------
#
#     if (plot_type == "Normal") {
#       mu <- input$mu
#       sigma <- input$sig
#       xbar <- input$xbar
#       alpha <- input$alpha
#       lb <- mu - 4 * sigma
#       ub <- mu + 4 * sigma
#       ggplot() +
#         stat_function(fun = dnorm, args = list(mean = mu, sd = sigma)) +
#         lims(x = c(lb, ub)) +
#         stat_function(
#           fun = dnorm, args = list(mean = mu, sd = sigma),
#           geom = "area", xlim = c(qnorm(1 - alpha, mu, sigma), ub),
#           alpha = 0.75, fill = "brown"
#         ) +
#         geom_point(aes(x = xbar, y = 0.0125), shape = 6, size = 7) +
#         stat_function(
#           fun = dnorm, args = list(mean = mu, sd = sigma),
#           geom = "area", xlim = c(xbar, ub),
#           alpha = 0.5, fill = "grey"
#         )
#
#
# # t Distribution ----------------------------------------------------------
#
#     } else if (plot_type == "t") {
#       df <- as.numeric(input$dft)
#       ggplot() +
#         stat_function(fun = dt, args = list(df = df)) +
#         lims(x = c(-4, 4))
#
#
# # Chi Square Distribution -------------------------------------------------
#
#     } else if (plot_type == "Chi Square") {
#       df <- as.numeric(input$dfc)
#       ggplot() +
#         stat_function(fun = dchisq, args = list(df = df)) +
#         lims(x = c(0, qchisq(1 - .Machine$double.eps, df)))
#
#
# # F Distribution ----------------------------------------------------------
#
#     } else if (plot_type == "F") {
#       df1 <- as.numeric(input$df1)
#       df2 <- as.numeric(input$df2)
#
#       ggplot() +
#         stat_function(fun = df, args = list(df1 = df1, df2 = df2)) +
#         lims(x = c(0, qf(0.999, df1, df2)))
#     }
  })


# Results -----------------------------------------------------------------

  output$text <- renderUI({
    test_type <- input$test
    if (test_type == "One Sample Proportion") {
      p <- input$p
      n <- input$n
      sigma <- sqrt(p * (1 - p) / n)
      phat <- input$phat
      alpha <- input$alpha
      lb <- p - 4 * sigma
      ub <- p + 4 * sigma
      hypothesis_type <- input$hypot

      withMathJax(
        sprintf(
          '$$P\\left(\\hat{p}\\leq %.02f \\right) = %.04f$$',
          phat, pnorm(phat, p, sigma)
        )
      )
    }
  })
}
