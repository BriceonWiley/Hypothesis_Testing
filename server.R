# Cmd + Shift + Enter to run App

server <- function(input, output) {

  update_plot <- eventReactive(input$update, {
    # Plot ----------------------------------------------------------------
    test_type <- input$test

    if (test_type == "One Proportion") {
      # One Sample Proportion ---------------------------------------------
      # necessary set up
      p <- input$p
      n <- input$np
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

      if (hypothesis_type == '<') {
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
          ) -> one_samp_prop
      } else if (hypothesis_type == '>') {
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
          ) -> one_samp_prop
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
          } -> one_samp_prop
      }
      one_samp_prop
    } else if (test_type == 'One Mean') {
      # One Sample Mean ---------------------------------------------------
      # necessary set up
      mu <- input$mu
      n <- input$nmu
      std <- input$sig / sqrt(n)
      xbar <- input$xbar
      alpha <- input$alpha
      lb <- mu - 4 * std
      ub <- mu + 4 * std
      z <- (xbar - mu) / std
      df <- n - 1
      hypothesis_type <- input$alternative

      if (input$std_src == 'Population') {
        # base plot
        one_samp_mean <- ggplot() +
          stat_function(fun = dnorm, args = list(mean = mu, sd = std)) +
          geom_point(aes(x = xbar, y = 0.0425 * dnorm(mu, mu, std)), shape = 6, size = 7) +
          geom_point(aes(x = mu, y = -0.0425 * dnorm(mu, mu, std)), shape = 17, size = 7) +
          lims(x = c(lb, ub)) +
          theme_minimal(21) +
          labs(x = "Sample Mean", y = "Probability")

        if (hypothesis_type == '<') {
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
            ) -> one_samp_mean
        } else if (hypothesis_type == '>') {
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
            ) -> one_samp_mean
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
            } -> one_samp_mean
        }
      } else if (input$std_src == 'Sample') {
        # base plot
        one_samp_mean <- ggplot() +
          stat_function(fun = dt, args = list(df = df)) +
          geom_point(aes(x = z, y = 0.0425 * dt(0, df)), shape = 6, size = 7) +
          geom_point(aes(x = 0, y = -0.0425 * dt(0, df)), shape = 17, size = 7) +
          lims(x = c(-4, 4)) +
          theme_minimal(21) +
          labs(x = "Sample Mean", y = "Probability")

        if (hypothesis_type == '<') {
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
            ) -> one_samp_mean
        } else if (hypothesis_type == '>') {
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
            ) -> one_samp_mean
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
            } -> one_samp_mean
        }
      }
      one_samp_mean
    } else if (test_type == 'Dependent Samples') {
      # Dependent Mean ----------------------------------------------------
      D0 <- input$D0
      n <- input$nd
      std <- input$sigd / sqrt(n)
      dbar <- input$dbar
      alpha <- input$alpha
      lb <- D0 - 4 * std
      ub <- D0 + 4 * std
      t <- (dbar - D0) / std
      df <- n - 1
      hypothesis_type <- input$alternative

      # base plot
      dependent_samples <- ggplot() +
        stat_function(fun = dt, args = list(df = df)) +
        geom_point(aes(x = t, y = 0.0425 * dt(0, df)), shape = 6, size = 7) +
        geom_point(aes(x = 0, y = -0.0425 * dt(0, df)), shape = 17, size = 7) +
        lims(x = c(-4, 4)) +
        theme_minimal(21) +
        labs(x = "Sample Difference", y = "Probability")

      if (hypothesis_type == '<') {
        # less than or equal to
        dependent_samples +
          stat_function(
            fun = dt, args = list(df = df),
            geom = "area", xlim = c(-4, qt(alpha, df)),
            alpha = 0.75, fill = "#A47551"
          ) +
          stat_function(
            fun = dt, args = list(df = df),
            geom = "area", xlim = c(-4, t),
            alpha = 0.5, fill = "grey"
          ) -> dependent_samples
      } else if (hypothesis_type == '>') {
        # greater than or equal to
        dependent_samples +
          stat_function(
            fun = dt, args = list(df = df),
            geom = "area", xlim = c(qt(1 - alpha, df), 4),
            alpha = 0.75, fill = "#A47551"
          ) +
          stat_function(
            fun = dt, args = list(df = df),
            geom = "area", xlim = c(t, 4),
            alpha = 0.5, fill = "grey"
          ) -> dependent_samples
      } else if (hypothesis_type == '≠') {
        # not equal to
        dependent_samples +
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
            if (dbar >= D0) {
              stat_function(
                fun = dt, args = list(df = df),
                geom = "area", xlim = c(t, 4),
                alpha = 0.5, fill = "grey"
              )
            } else {
              stat_function(
                fun = dt, args = list(df = df),
                geom = "area", xlim = c(-4, t),
                alpha = 0.5, fill = "grey"
              )
            }
          } -> dependent_samples
      }
      dependent_samples
    } else if (test_type == 'Independent Samples') {
      # Independent Mean --------------------------------------------------
    } else if (test_type == 'Two Proportions') {
      # Two Proportions ---------------------------------------------------
    }
  })

  update_distribution <- eventReactive(input$update, {
    # Distribution --------------------------------------------------------
    test_type <- input$test

    if (test_type == 'One Proportion') {
      # One Sample Proportion ---------------------------------------------
      p <- input$p
      n <- input$np
      sigma <- sqrt(p * (1 - p) / n)
      phat <- input$phat
      z <- (phat - p) / sigma
      withMathJax(
        sprintf(
          '$$\\hat{p}\\dot\\sim\\mathcal{N}\\left(%.02f,
          \\sqrt{\\frac{%0.2f*%0.2f}{%0.f}}=%.03f\\right)\\rightarrow
          Z=\\frac{%.02f-%0.2f}{%.03f}=%.02f$$',
          p, p, 1 - p, n, sigma, phat, p, sigma, z
        )
      )
    } else if (test_type == 'One Mean') {
      # One Sample Mean ---------------------------------------------------
      mu <- input$mu
      n <- input$nmu
      sigma <- input$sig
      std <- input$sig / sqrt(n)
      xbar <- input$xbar
      zt <- (xbar - mu) / std
      df <- n - 1
      if (input$std_src == 'Population') {
        withMathJax(
          sprintf(
            '$$\\bar{x}\\dot\\sim\\mathcal{N}\\left(%.02f,
            \\frac{%0.2f}{\\sqrt{%0.f}}=%.03f\\right)\\rightarrow
            Z=\\frac{%.02f-%0.2f}{%.02f/\\sqrt{%0.f}}=%.02f$$',
            mu, sigma, n, std, xbar, mu, sigma, n, zt
          )
        )
      } else if (input$std_src == 'Sample') {
        withMathJax(
          sprintf(
            '$$T=\\frac{\\bar{x}-\\mu}{s/\\sqrt{n}}\\sim t_{%0.f}\\rightarrow
            T=\\frac{%.02f-%0.2f}{%.02f/\\sqrt{%0.f}}=%.02f$$',
            df, xbar, mu, sigma, n, zt
          )
        )
      }
    } else if (test_type == 'Dependent Samples') {
      # Dependent Mean ----------------------------------------------------
      D0 <- input$D0
      n <- input$nd
      sigma <- input$sigd
      std <- input$sigd / sqrt(n)
      dbar <- input$dbar
      t <- (dbar - D0) / std
      df <- n - 1
      withMathJax(
        sprintf(
          '$$T=\\frac{\\bar{d}-D_0}{s_d/\\sqrt{n}}\\sim t_{%0.f}\\rightarrow
          T=\\frac{%.02f-%0.2f}{%.02f/\\sqrt{%0.f}}=%.02f$$',
          df, dbar, D0, sigma, n, t
        )
      )
    } else if (test_type == 'Independent Samples') {
      # Independent Mean --------------------------------------------------
    } else if (test_type == 'Two Proportions') {
      # Two Proportions ---------------------------------------------------
    }
  })

  update_pvalue_stat <- eventReactive(input$update, {
    # P-Value Data --------------------------------------------------------
    test_type <- input$test
    alpha <- input$alpha
    hypothesis_type <- input$alternative

    if (test_type == "One Proportion") {
      # One Sample Proportion ---------------------------------------------
      p <- input$p
      n <- input$np
      sigma <- sqrt(p * (1 - p) / n)
      phat <- input$phat

      if (hypothesis_type == '<') {
        # less than or equal to
        pvalue <- pnorm(phat, p, sigma)
        withMathJax(
          sprintf(
            '$$P\\left(\\hat{p}\\leq %.02f \\right) = %.04f$$',
            phat, pvalue
          )
        )

      } else if (hypothesis_type == '>') {
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
    } else if (test_type == 'One Mean') {
      # One Sample Mean ---------------------------------------------------
      mu <- input$mu
      n <- input$nmu
      sigma <- input$sig
      std <- input$sig / sqrt(n)
      xbar <- input$xbar
      z <- (xbar - mu) / std
      df <- n - 1
      if (input$std_src == 'Population') {
        if (hypothesis_type == '<') {
          # less than or equal to
          pvalue <- pnorm(xbar, mu, std)
          withMathJax(
            sprintf(
              '$$P\\left(\\bar{x}\\leq %.02f \\right) = %.04f$$',
              xbar, pvalue
            )
          )

        } else if (hypothesis_type == '>') {
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
      } else if (input$std_src == 'Sample') {
        if (hypothesis_type == '<') {
          # less than or equal to
          pvalue <- pt(z, df)
          withMathJax(
            sprintf(
              '$$P\\left(\\bar{x}\\leq %.02f \\right) = %.04f$$',
              xbar, pvalue
            )
          )

        } else if (hypothesis_type == '>') {
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
    } else if (test_type == 'Dependent Samples') {
      # Dependent Mean ----------------------------------------------------
      D0 <- input$D0
      n <- input$nd
      sigma <- input$sigd
      std <- input$sigd / sqrt(n)
      dbar <- input$dbar
      t <- (dbar - D0) / std
      df <- n - 1

      if (hypothesis_type == '<') {
        # less than or equal to
        pvalue <- pt(t, df)
        withMathJax(
          sprintf(
            '$$P\\left(\\bar{d}\\leq %.02f \\right) = %.04f$$',
            dbar, pvalue
          )
        )

      } else if (hypothesis_type == '>') {
        # greater than or equal to
        pvalue <- 1 - pt(t, df)
        withMathJax(
          sprintf(
            '$$P\\left(\\bar{d}\\geq %.02f \\right) = 1-P\\left(\\bar{d}< %.02f \\right) = %.04f$$',
            dbar, dbar, pvalue
          )
        )

      } else if (hypothesis_type == '≠') {
        # not equal to
        if (dbar >= D0) {
          pvalue <- 2 * (1 - pt(t, df))
          withMathJax(
            sprintf(
              '$$P\\left(|\\bar{d}|\\geq %.02f \\right) = 2*\\left(1-P\\left(\\bar{d}\\leq %.02f \\right)\\right) = %.04f$$',
              dbar, dbar, pvalue
            )
          )
        } else {
          pvalue <- 2 * pt(t, df)
          withMathJax(
            sprintf(
              '$$P\\left(|\\bar{d}|\\leq %.02f \\right) = 2*P\\left(\\bar{d}\\leq %.02f \\right) = %.04f$$',
              dbar, dbar, pvalue
            )
          )
        }
      }
    } else if (test_type == 'Independent Samples') {
      # Independent Mean --------------------------------------------------
    } else if (test_type == 'Two Proportions') {
      # Two Proportions ---------------------------------------------------
    }
  })

  update_pvalue_z <- eventReactive(input$update, {
    # P-Value Test Stat ---------------------------------------------------
    test_type <- input$test
    alpha <- input$alpha
    hypothesis_type <- input$alternative

    if (test_type == "One Proportion") {
      # One Sample Proportion ---------------------------------------------
      p <- input$p
      n <- input$np
      sigma <- sqrt(p * (1 - p) / n)
      phat <- input$phat
      z <- (phat - p) / sigma

      if (hypothesis_type == '<') {
        # less than or equal to
        pvalue <- pnorm(z)
        withMathJax(
          sprintf(
            '$$P\\left(Z\\leq %.02f \\right) = %.04f$$',
            z, pvalue
          )
        )

      } else if (hypothesis_type == '>') {
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
    } else if (test_type == 'One Mean') {
      # One Sample Mean ---------------------------------------------------
      mu <- input$mu
      n <- input$nmu
      sigma <- input$sig
      std <- sigma / sqrt(n)
      xbar <- input$xbar
      z <- (xbar - mu) / std
      df <- n - 1

      if (input$std_src == 'Population') {
        if (hypothesis_type == '<') {
          # less than or equal to
          pvalue <- pnorm(z)
          withMathJax(
            sprintf(
              '$$P\\left(Z\\leq %.02f \\right) = %.04f$$',
              z, pvalue
            )
          )

        } else if (hypothesis_type == '>') {
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
          if (xbar >= mu) {
            pvalue <- 2 * (1 - pnorm(z))
            withMathJax(
              sprintf(
                '$$P\\left(|Z|\\geq %.02f \\right) = 2*\\left(1-P\\left(Z\\leq %.02f \\right)\\right) = %.04f$$',
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
      } else if (input$std_src == 'Sample') {
        if (hypothesis_type == '<') {
          # less than or equal to
          pvalue <- pt(z, df)
          withMathJax(
            sprintf(
              '$$P\\left(T\\leq %.02f \\right) = %.04f$$',
              z, pvalue
            )
          )

        } else if (hypothesis_type == '>') {
          # greater than or equal to
          pvalue <- 1 - pt(z, df)
          withMathJax(
            sprintf(
              '$$P\\left(T\\geq %.02f \\right) = 1-P\\left(T< %.02f \\right) = %.04f$$',
              z, z, pvalue
            )
          )

        } else if (hypothesis_type == '≠') {
          # not equal to
          if (xbar >= mu) {
            pvalue <- 2 * (1 - pt(z, df))
            withMathJax(
              sprintf(
                '$$P\\left(|T|\\geq %.02f \\right) = 2*\\left(1-P\\left(T\\leq %.02f \\right)\\right) = %.04f$$',
                z, z, pvalue
              )
            )
          } else {
            pvalue <- 2 * (pt(z, df))
            withMathJax(
              sprintf(
                '$$P\\left(|T|\\leq %.02f \\right) = 2*P\\left(T\\leq %.02f \\right) = %.04f$$',
                z, z, pvalue
              )
            )
          }
        }
      }
    } else if (test_type == 'Dependent Samples') {
      # Dependent Mean ----------------------------------------------------
      D0 <- input$D0
      n <- input$nd
      sigma <- input$sigd
      std <- input$sigd / sqrt(n)
      dbar <- input$dbar
      t <- (dbar - D0) / std
      df <- n - 1

      if (hypothesis_type == '<') {
        # less than or equal to
        pvalue <- pt(t, df)
        withMathJax(
          sprintf(
            '$$P\\left(T\\leq %.02f \\right) = %.04f$$',
            t, pvalue
          )
        )

      } else if (hypothesis_type == '>') {
        # greater than or equal to
        pvalue <- 1 - pt(t, df)
        withMathJax(
          sprintf(
            '$$P\\left(T\\geq %.02f \\right) = 1-P\\left(T< %.02f \\right) = %.04f$$',
            t, t, pvalue
          )
        )

      } else if (hypothesis_type == '≠') {
        # not equal to
        if (dbar >= D0) {
          pvalue <- 2 * (1 - pt(t, df))
          withMathJax(
            sprintf(
              '$$P\\left(|T|\\geq %.02f \\right) = 2*\\left(1-P\\left(T\\leq %.02f \\right)\\right) = %.04f$$',
              t, t, pvalue
            )
          )
        } else {
          pvalue <- 2 * (pt(t, df))
          withMathJax(
            sprintf(
              '$$P\\left(|T|\\leq %.02f \\right) = 2*P\\left(T\\leq %.02f \\right) = %.04f$$',
              t, t, pvalue
            )
          )
        }
      }
    } else if (test_type == 'Independent Samples') {
      # Independent Mean --------------------------------------------------
    } else if (test_type == 'Two Proportions') {
      # Two Proportions ---------------------------------------------------
    }
  })

  update_ci <- eventReactive(input$update, {
    # CI ------------------------------------------------------------------
    test_type <- input$test
    alpha <- input$alpha
    hypothesis_type <- input$alternative

    if (test_type == 'One Proportion') {
      # One Sample Proportion ---------------------------------------------
      phat <- input$phat
      n <- input$np
      stderr <- sqrt(phat * (1 - phat) / n)

      if (hypothesis_type == '<') {
        # less than or equal to
        crit <- qnorm(1 - alpha)
        ub <- phat + crit * stderr
        withMathJax(
          sprintf(
            '$$\\hat{p}+Z_{\\alpha}\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}
            =%.02f+%.02f\\sqrt{\\frac{%.02f(%.02f)}{%.0f}}
            \\implies p\\leq%0.4f$$',
            phat, crit, phat, 1 - phat, n, ub
          )
        )
      } else if (hypothesis_type == '>') {
        # greater than or equal to
        crit <- qnorm(1 - alpha)
        lb <- phat - crit * stderr
        withMathJax(
          sprintf(
            '$$\\hat{p}+Z_{\\alpha}\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}
            =%.02f-%.02f\\sqrt{\\frac{%.02f(%.02f)}{%.0f}}
            \\implies p\\geq%0.4f$$',
            phat, crit, phat, 1 - phat, n, lb
          )
        )
      } else if (hypothesis_type == '≠') {
        # not equal to
        crit <- qnorm(1 - alpha / 2)
        lb <- phat - crit * stderr
        ub <- phat + crit * stderr
        withMathJax(
          sprintf(
            '$$\\hat{p}\\pm Z_{\\alpha/2}\\sqrt{\\frac{\\hat{p}(1-\\hat{p})}{n}}
            =%.02f\\pm %.02f\\sqrt{\\frac{%.02f*%.02f}{%.0f}}
            \\implies p\\in\\left(%.04f,%0.4f\\right)$$',
            phat, crit, phat, 1 - phat, n, lb, ub
          )
        )
      }
    } else if (test_type == 'One Mean') {
      # One Sample Mean ---------------------------------------------------
      xbar <- input$xbar
      n <- input$nmu
      sig <- input$sig
      stderr <- sig / sqrt(n)

      if (input$std_src == 'Population') {
        if (hypothesis_type == '<') {
          # less than or equal to
          crit <- qnorm(1 - alpha)
          ub <- xbar + crit * stderr
          withMathJax(
            sprintf(
              '$$\\bar{x}+Z_{\\alpha}\\frac{\\sigma}{\\sqrt{n}}
              =%0.2f+%0.2f\\frac{%0.2f}{\\sqrt{%0.0f}}
              \\implies \\mu\\leq%0.4f$$',
              xbar, crit, sig, n, ub
            )
          )
        } else if (hypothesis_type == '>') {
          # greater than or equal to
          crit <- qnorm(1 - alpha)
          lb <- xbar - crit * stderr
          withMathJax(
            sprintf(
              '$$\\bar{x}-Z_{\\alpha}\\frac{\\sigma}{\\sqrt{n}}
              =%0.2f-%0.2f\\frac{%0.2f}{\\sqrt{%0.0f}}
              \\implies \\mu\\geq%0.4f$$',
              xbar, crit, sig, n, lb
            )
          )
        } else if (hypothesis_type == '≠') {
          # not equal to
          crit <- qnorm(1 - alpha / 2)
          lb <- xbar - crit * stderr
          ub <- xbar + crit * stderr
          withMathJax(
            sprintf(
              '$$\\bar{x}\\pm Z_{\\alpha/2}\\frac{\\sigma}{\\sqrt{n}}
              =%0.2f\\pm%0.2f\\frac{%0.2f}{\\sqrt{%0.0f}}
              \\implies \\mu\\in\\left(%0.4f,%0.4f\\right)$$',
              xbar, crit, sig, n, lb, ub
            )
          )
        }
      } else if (input$std_src == 'Sample') {
        df <- n - 1

        if (hypothesis_type == '<') {
          # less than or equal to
          crit <- qt(1 - alpha, df)
          ub <- xbar + crit * stderr
          withMathJax(
            sprintf(
              '$$\\bar{x}+t_{\\alpha}\\frac{s}{\\sqrt{n}}
              =%0.2f+%0.2f\\frac{%0.2f}{\\sqrt{%0.0f}}
              \\implies \\mu\\leq%0.4f$$',
              xbar, crit, sig, n, ub
            )
          )
        } else if (hypothesis_type == '>') {
          # greater than or equal to
          crit <- qt(1 - alpha, df)
          lb <- xbar - crit * stderr
          withMathJax(
            sprintf(
              '$$\\bar{x}-t_{\\alpha}\\frac{s}{\\sqrt{n}}
              =%0.2f-%0.2f\\frac{%0.2f}{\\sqrt{%0.0f}}
              \\implies \\mu\\geq%0.4f$$',
              xbar, crit, sig, n, lb
            )
          )
        } else if (hypothesis_type == '≠') {
          # not equal to
          crit <- qt(1 - alpha / 2, df)
          lb <- xbar - crit * stderr
          ub <- xbar + crit * stderr
          withMathJax(
            sprintf(
              '$$\\bar{x}\\pm t_{\\alpha/2}\\frac{s}{\\sqrt{n}}
              =%0.2f\\pm%0.2f\\frac{%0.2f}{\\sqrt{%0.0f}}
              \\implies \\mu\\in\\left(%0.4f,%0.4f\\right)$$',
              xbar, crit, sig, n, lb, ub
            )
          )
        }
      }
    } else if (test_type == 'Dependent Samples') {
      # Dependent Mean ----------------------------------------------------
      dbar <- input$dbar
      n <- input$nd
      sig <- input$sigd
      stderr <- sig / sqrt(n)
      df <- n - 1

      if (hypothesis_type == '<') {
        # less than or equal to
        crit <- qt(1 - alpha, df)
        ub <- dbar + crit * stderr
        withMathJax(
          sprintf(
            '$$\\bar{d}+t_{\\alpha}\\frac{s_d}{\\sqrt{n}}
            =%0.2f+%0.2f\\frac{%0.2f}{\\sqrt{%0.0f}}
            \\implies \\mu_d\\leq%0.4f$$',
            dbar, crit, sig, n, ub
          )
        )
      } else if (hypothesis_type == '>') {
        # greater than or equal to
        crit <- qt(1 - alpha, df)
        lb <- dbar - crit * stderr
        withMathJax(
          sprintf(
            '$$\\bar{d}-t_{\\alpha}\\frac{s_d}{\\sqrt{n}}
            =%0.2f-%0.2f\\frac{%0.2f}{\\sqrt{%0.0f}}
            \\implies \\mu_d\\geq%0.4f$$',
            dbar, crit, sig, n, lb
          )
        )
      } else if (hypothesis_type == '≠') {
        # not equal to
        crit <- qt(1 - alpha / 2, df)
        lb <- dbar - crit * stderr
        ub <- dbar + crit * stderr
        withMathJax(
          sprintf(
            '$$\\bar{d}\\pm t_{\\alpha/2}\\frac{s_d}{\\sqrt{n}}
            =%0.2f\\pm%0.2f\\frac{%0.2f}{\\sqrt{%0.0f}}
            \\implies \\mu_d\\in\\left(%0.4f,%0.4f\\right)$$',
            dbar, crit, sig, n, lb, ub
          )
        )
      }
    } else if (test_type == 'Independent Samples') {
      # Independent Mean --------------------------------------------------
    } else if (test_type == 'Two Proportions') {
      # Two Proportions ---------------------------------------------------
    }
  })


# Display Plots -----------------------------------------------------------

  output$distPlot <- renderPlot({
    update_plot()
  })


# Results -----------------------------------------------------------------

  output$distribution <- renderUI({
    update_distribution()
  })

  output$pvalue_stat <- renderUI({
    update_pvalue_stat()
  })

  output$pvalue_z <- renderUI({
    update_pvalue_z()
  })

  output$CI <- renderUI({
    update_ci()
  })
}
