# Cmd + Shift + Enter to run App

server <- function(input, output) {


# Plots -------------------------------------------------------------------

  output$distPlot <- renderPlot({
    test_type <- input$test


    if (test_type == "One Sample Proportion") {
# One Sample Proportion ---------------------------------------------------
      # necessary set up
      p <- input$p
      n <- input$n
      sigma <- sqrt(p * (1 - p) / n)
      phat <- input$phat
      alpha <- input$alpha
      lb <- p - 4 * sigma
      ub <- p + 4 * sigma
      hypothesis_type <- input$alternative

      # base plot
      one_samp_prop <- ggplot() +
        stat_function(fun = dnorm, args = list(mean = p, sd = sigma)) +
        geom_point(aes(x = phat, y = 0.0425 * dnorm(p, p, sigma)), shape = 6, size = 7) +
        geom_point(aes(x = p, y = -0.0425 * dnorm(p, p, sigma)), shape = 17, size = 7) +
        lims(x = c(lb, ub)) +
        theme_minimal(21) +
        labs(x = "Sample Proporiton", y = "Probability")

      if (hypothesis_type == '≤') {
        # less than or equal to
        one_samp_prop +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(lb, qnorm(alpha, p, sigma)),
            alpha = 0.75, fill = "#A47551"
          ) +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(lb, phat),
            alpha = 0.5, fill = "grey"
          )
      } else if (hypothesis_type == '≥') {
        # greater than or equal to
        one_samp_prop +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(qnorm(1 - alpha, p, sigma), ub),
            alpha = 0.75, fill = "#A47551"
          ) +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(phat, ub),
            alpha = 0.5, fill = "grey"
          )
      } else if (hypothesis_type == '≠') {
        # not equal to
        one_samp_prop +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(lb, qnorm(alpha / 2, p, sigma)),
            alpha = 0.75, fill = "#A47551"
          ) +
          stat_function(
            fun = dnorm, args = list(mean = p, sd = sigma),
            geom = "area", xlim = c(qnorm(1 - alpha / 2, p, sigma), ub),
            alpha = 0.75, fill = "#A47551"
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
    } else if (test_type == 'One Sample Mean') {
# One Sample Mean ---------------------------------------------------------
      # necessary set up
      mu <- input$mu
      n <- input$n
      std <- input$sig / sqrt(n)
      xbar <- input$xbar
      alpha <- input$alpha
      lb <- mu - 4 * std
      ub <- mu + 4 * std
      z <- (xbar - mu) / std
      df <- n - 1
      hypothesis_type <- input$alternative

      if (input$pop_std) {
        # base plot
        one_samp_mean <- ggplot() +
          stat_function(fun = dnorm, args = list(mean = mu, sd = std)) +
          geom_point(aes(x = xbar, y = 0.0425 * dnorm(mu, mu, std)), shape = 6, size = 7) +
          geom_point(aes(x = mu, y = -0.0425 * dnorm(mu, mu, std)), shape = 17, size = 7) +
          lims(x = c(lb, ub)) +
          theme_minimal(21) +
          labs(x = "Sample Mean", y = "Probability")

        if (hypothesis_type == '≤') {
          # less than or equal to
          one_samp_mean +
            stat_function(
              fun = dnorm, args = list(mean = mu, sd = std),
              geom = "area", xlim = c(lb, qnorm(alpha, mu, std)),
              alpha = 0.75, fill = "#A47551"
            ) +
            stat_function(
              fun = dnorm, args = list(mean = mu, sd = std),
              geom = "area", xlim = c(lb, xbar),
              alpha = 0.5, fill = "grey"
            )
        } else if (hypothesis_type == '≥') {
          # greater than or equal to
          one_samp_mean +
            stat_function(
              fun = dnorm, args = list(mean = mu, sd = std),
              geom = "area", xlim = c(qnorm(1 - alpha, mu, std), ub),
              alpha = 0.75, fill = "#A47551"
            ) +
            stat_function(
              fun = dnorm, args = list(mean = mu, sd = std),
              geom = "area", xlim = c(xbar, ub),
              alpha = 0.5, fill = "grey"
            )
        } else if (hypothesis_type == '≠') {
          # not equal to
          one_samp_mean +
            stat_function(
              fun = dnorm, args = list(mean = mu, sd = std),
              geom = "area", xlim = c(lb, qnorm(alpha / 2, mu, std)),
              alpha = 0.75, fill = "#A47551"
            ) +
            stat_function(
              fun = dnorm, args = list(mean = mu, sd = std),
              geom = "area", xlim = c(qnorm(1 - alpha / 2, mu, std), ub),
              alpha = 0.75, fill = "#A47551"
            ) + {
              if (xbar >= mu) {
                stat_function(
                  fun = dnorm, args = list(mean = mu, sd = std),
                  geom = "area", xlim = c(xbar, ub),
                  alpha = 0.5, fill = "grey"
                )
              } else {
                stat_function(
                  fun = dnorm, args = list(mean = mu, sd = std),
                  geom = "area", xlim = c(lb, xbar),
                  alpha = 0.5, fill = "grey"
                )
              }
            }
        }
      } else {
        # base plot
        one_samp_mean <- ggplot() +
          stat_function(fun = dt, args = list(df = df)) +
          geom_point(aes(x = z, y = 0.0425 * dt(0, df)), shape = 6, size = 7) +
          geom_point(aes(x = 0, y = -0.0425 * dt(0, df)), shape = 17, size = 7) +
          lims(x = c(-4, 4)) +
          theme_minimal(21) +
          labs(x = "Sample Mean", y = "Probability")

        if (hypothesis_type == '≤') {
          # less than or equal to
          one_samp_mean +
            stat_function(
              fun = dt, args = list(df = df),
              geom = "area", xlim = c(-4, qt(alpha, df)),
              alpha = 0.75, fill = "#A47551"
            ) +
            stat_function(
              fun = dt, args = list(df = df),
              geom = "area", xlim = c(-4, z),
              alpha = 0.5, fill = "grey"
            )
        } else if (hypothesis_type == '≥') {
          # greater than or equal to
          one_samp_mean +
            stat_function(
              fun = dt, args = list(df = df),
              geom = "area", xlim = c(qt(1 - alpha, df), 4),
              alpha = 0.75, fill = "#A47551"
            ) +
            stat_function(
              fun = dt, args = list(df = df),
              geom = "area", xlim = c(z, 4),
              alpha = 0.5, fill = "grey"
            )
        } else if (hypothesis_type == '≠') {
          # not equal to
          one_samp_mean +
            stat_function(
              fun = dt, args = list(df = df),
              geom = "area", xlim = c(-4, qt(alpha / 2, df)),
              alpha = 0.75, fill = "#A47551"
            ) +
            stat_function(
              fun = dt, args = list(df = df),
              geom = "area", xlim = c(qt(1 - alpha / 2, df), 4),
              alpha = 0.75, fill = "#A47551"
            ) + {
              if (xbar >= mu) {
                stat_function(
                  fun = dt, args = list(df = df),
                  geom = "area", xlim = c(z, 4),
                  alpha = 0.5, fill = "grey"
                )
              } else {
                stat_function(
                  fun = dt, args = list(df = df),
                  geom = "area", xlim = c(-4, z),
                  alpha = 0.5, fill = "grey"
                )
              }
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

  output$distribution <- renderUI({
    test_type <- input$test

    if (test_type == 'One Sample Proportion') {
      p <- input$p
      n <- input$n
      sigma <- sqrt(p * (1 - p) / n)
      phat <- input$phat
      withMathJax(
        sprintf(
          '$$\\hat{p}\\dot\\sim\\mathcal{N}\\left(%.02f,\\sqrt{\\frac{%0.2f*%0.2f}{%0.f}}=%.03f\\right)$$',
          phat, p, p, n, sigma
        )
      )
    } else if (test_type == 'One Sample Mean') {
      mu <- input$mu
      n <- input$n
      sigma <- input$sig
      std <- input$sig / sqrt(n)
      xbar <- input$xbar
      z <- (xbar - mu) / std
      df <- n - 1
      if (input$pop_std) {
        withMathJax(
          sprintf(
            '$$\\bar{x}\\dot\\sim\\mathcal{N}\\left(%.02f,\\frac{%0.2f}{\\sqrt{%0.f}}=%.03f\\right)$$',
            mu, sigma, n, std
          )
        )
      } else {
        withMathJax(
          sprintf(
            '$$T=\\frac{\\bar{x}-\\mu}{s/\\sqrt{n}}\\sim t_{%0.f}$$',
            df
          )
        )
      }
    }
  })

  output$transform <- renderUI({
    test_type <- input$test

    if (test_type == 'One Sample Proportion') {
      p <- input$p
      n <- input$n
      sigma <- sqrt(p * (1 - p) / n)
      phat <- input$phat
      z <- (phat - p) / sigma
      withMathJax(
        sprintf(
          '$$Z=\\frac{%.02f-%0.2f}{%.03f}=%.02f$$',
          phat, p, sigma, z
        )
      )
    } else if (test_type == 'One Sample Mean') {
      mu <- input$mu
      n <- input$n
      sigma <- input$sig
      std <- input$sig / sqrt(n)
      xbar <- input$xbar
      z <- (xbar - mu) / std
      df <- n - 1
      if (input$pop_std) {
        withMathJax(
          sprintf(
            '$$Z=\\frac{%.02f-%0.2f}{%.02f/\\sqrt{%0.f}}=%.02f$$',
            xbar, mu, sigma, n, z
          )
        )
      } else {
        withMathJax(
          sprintf(
            '$$T=\\frac{%.02f-%0.2f}{%.02f/\\sqrt{%0.f}}=%.02f$$',
            xbar, mu, sigma, n, z
          )
        )
      }
    }
  })

  output$pvalue_stat <- renderUI({
    test_type <- input$test
    alpha <- input$alpha
    hypothesis_type <- input$alternative

    if (test_type == "One Sample Proportion") {
      p <- input$p
      n <- input$n
      sigma <- sqrt(p * (1 - p) / n)
      phat <- input$phat

      if (hypothesis_type == '≤') {
        # less than or equal to
        pvalue <- pnorm(phat, p, sigma)
        withMathJax(
          sprintf(
            '$$P\\left(\\hat{p}\\leq %.02f \\right) = %.04f$$',
            phat, pvalue
          )
        )

      } else if (hypothesis_type == '≥') {
        # greater than or equal to
        pvalue <- 1 - pnorm(phat, p, sigma)
        withMathJax(
          sprintf(
            '$$P\\left(\\hat{p}\\geq %.02f \\right) = 1-P\\left(\\hat{p}< %.02f \\right) = %.04f$$',
            phat, phat, pvalue
          )
        )

      } else if (hypothesis_type == '≠') {
        # not equal to
        if (phat >= p) {
          pvalue <- 2 * (1 - pnorm(phat, p, sigma))
          withMathJax(
            sprintf(
              '$$P\\left(|\\hat{p}|\\geq %.02f \\right) = 2*\\left(1-P\\left(\\hat{p}\\leq %.02f \\right)\\right) = %.04f$$',
              phat, phat, pvalue
            )
          )
        } else {
          pvalue <- 2 * (pnorm(phat, p, sigma))
          withMathJax(
            sprintf(
              '$$P\\left(|\\hat{p}|\\leq %.02f \\right) = 2*P\\left(\\hat{p}\\leq %.02f \\right) = %.04f$$',
              phat, phat, pvalue
            )
          )
        }
      }
    } else if (test_type == 'One Sample Mean') {
      mu <- input$mu
      n <- input$n
      sigma <- input$sig
      std <- input$sig / sqrt(n)
      xbar <- input$xbar
      z <- (xbar - mu) / std
      df <- n - 1

      if (input$pop_std) {
        if (hypothesis_type == '≤') {
          # less than or equal to
          pvalue <- pnorm(xbar, mu, std)
          withMathJax(
            sprintf(
              '$$P\\left(\\bar{x}\\leq %.02f \\right) = %.04f$$',
              xbar, pvalue
            )
          )

        } else if (hypothesis_type == '≥') {
          # greater than or equal to
          pvalue <- 1 - pnorm(xbar, mu, std)
          withMathJax(
            sprintf(
              '$$P\\left(\\bar{x}\\geq %.02f \\right) = 1-P\\left(\\bar{x}< %.02f \\right) = %.04f$$',
              xbar, xbar, pvalue
            )
          )

        } else if (hypothesis_type == '≠') {
          # not equal to
          if (xbar >= mu) {
            pvalue <- 2 * (1 - pnorm(xbar, mu, std))
            withMathJax(
              sprintf(
                '$$P\\left(|\\bar{x}|\\geq %.02f \\right) = 2*\\left(1-P\\left(\\bar{x}\\leq %.02f \\right)\\right) = %.04f$$',
                xbar, xbar, pvalue
              )
            )
          } else {
            pvalue <- 2 * (pnorm(xbar, mu, std))
            withMathJax(
              sprintf(
                '$$P\\left(|\\bar{x}|\\leq %.02f \\right) = 2*P\\left(\\bar{x}\\leq %.02f \\right) = %.04f$$',
                xbar, xbar, pvalue
              )
            )
          }
        }
      } else {
        if (hypothesis_type == '≤') {
          # less than or equal to
          pvalue <- pt(z, df)
          withMathJax(
            sprintf(
              '$$P\\left(\\bar{x}\\leq %.02f \\right) = %.04f$$',
              xbar, pvalue
            )
          )

        } else if (hypothesis_type == '≥') {
          # greater than or equal to
          pvalue <- 1 - pt(z, df)
          withMathJax(
            sprintf(
              '$$P\\left(\\bar{x}\\geq %.02f \\right) = 1-P\\left(\\bar{x}< %.02f \\right) = %.04f$$',
              xbar, xbar, pvalue
            )
          )

        } else if (hypothesis_type == '≠') {
          # not equal to
          if (xbar >= mu) {
            pvalue <- 2 * (1 - pt(z, df))
            withMathJax(
              sprintf(
                '$$P\\left(|\\bar{x}|\\geq %.02f \\right) = 2*\\left(1-P\\left(\\bar{x}\\leq %.02f \\right)\\right) = %.04f$$',
                xbar, xbar, pvalue
              )
            )
          } else {
            pvalue <- 2 * pt(z, df)
            withMathJax(
              sprintf(
                '$$P\\left(|\\bar{x}|\\leq %.02f \\right) = 2*P\\left(\\bar{x}\\leq %.02f \\right) = %.04f$$',
                xbar, xbar, pvalue
              )
            )
          }
        }
      }
    }
  })

  output$pvalue_z <- renderUI({

    test_type <- input$test
    p <- input$p
    n <- input$n
    sigma <- sqrt(p * (1 - p) / n)
    phat <- input$phat
    alpha <- input$alpha
    z <- (phat - p) / sigma
    hypothesis_type <- input$alternative

    if (test_type == "One Sample Proportion") {

      if (hypothesis_type == '≤') {
        # less than or equal to
        pvalue <- pnorm(z)
        withMathJax(
          sprintf(
            '$$P\\left(Z\\leq %.02f \\right) = %.04f$$',
            z, pvalue
          )
        )

      } else if (hypothesis_type == '≥') {
        # greater than or equal to
        pvalue <- 1 - pnorm(z)
        withMathJax(
          sprintf(
            '$$P\\left(Z\\geq %.02f \\right) = 1-P\\left(Z< %.02f \\right) = %.04f$$',
            z, z, pvalue
          )
        )

      } else if (hypothesis_type == '≠') {
        # not equal to
        if (z >= 0) {
          pvalue <- 2 * (1 - pnorm(z))
          withMathJax(
            sprintf(
              '$$P\\left(|Z|\\geq %.02f \\right) = 2*\\left(1-P\\left(Z\\geq %.02f \\right)\\right) = %.04f$$',
              z, z, pvalue
            )
          )
        } else {
          pvalue <- 2 * (pnorm(z))
          withMathJax(
            sprintf(
              '$$P\\left(|Z|\\leq %.02f \\right) = 2*P\\left(Z\\leq %.02f \\right) = %.04f$$',
              z, z, pvalue
            )
          )
        }
      }
    }
  })
}
