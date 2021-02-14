# Cmd + Shift + Enter to run App

library(shiny)
library(ggplot2)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Hyptothesis Testing"),


# Sidebar Design ----------------------------------------------------------

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "test", "Type of Test:",
        choices = c(
          "One Sample Proportion",
          "One Sample Mean",
          "Two Samples Mean"
        ), selected = 'One Sample Mean'
        # width = "50%"
      ),
      # selectInput(
      #   "test", "Type of Distribution:",
      #   choices = c("Normal", "t", "Chi Square", "F"),
      #   # width = "50%"
      # ),

      conditionalPanel(
        condition = "input.test == 'One Sample Proportion'",
        fixedRow(
          sliderInput("p", "Hypothesized Proportion:",
            min = 0, max = 1,
            value = 0.5, step = 0.01),
          # column(6,
          #   numericInput("p", "Hypothesized Proportion", value = 0.5)
          # ),
        ),
        fixedRow(
          radioButtons(
            'alternative', 'Alternative Hypothesis:',
            choices = c("≤", "≥", "≠"),
            inline = TRUE
          )
        ),
        fixedRow(
          numericInput("alpha", "α", value = 0.05, min = 0, max = 1, step = 0.01)
        ),
        fixedRow(
          numericInput("phat", "Sample Proportion", value = 0.5, min = 0, max = 1, step = 0.01)
        ),
        fixedRow(
          numericInput("n", "Sample Size", value = 50)
        )
      ),
      # conditionalPanel(
      #   condition = "input.test == 'Normal'",
      #   fixedRow(
      #     column(6,
      #       numericInput("xbar", "Sample Mean", value = 0)
      #     ),
      #     column(6,
      #       numericInput("alpha", "α", value = 0.05, min = 0, max = 1, step = 0.01)
      #     )
      #   )
      # ),

      conditionalPanel(
        condition = "input.test == 'One Sample Mean'",
        fixedRow(
          numericInput("mu", "Hypothesized Mean", value = 0, step = 0.5)
        ),
        fixedRow(
          radioButtons(
            'alternative', 'Alternative Hypothesis:',
            choices = c("≤", "≥", "≠"),
            inline = TRUE
          )
        ),
        fixedRow(
          switchInput(
            inputId = 'var', label = 'Standard Deviation', value = TRUE,
            onLabel = 'Population', offLabel = 'Sample', onStatus = 'success',
            offStatus = 'danger', inline = TRUE
          )
        ),
        fixedRow(
          numericInput("alpha", "α", value = 0.05, min = 0, max = 1, step = 0.01)
        ),
        fixedRow(
          numericInput("xbar", "Sample Mean", value = 0, step = 0.5)
        ),
        fixedRow(
          numericInput("sig", "Standard Deviation", value = 0, step = 0.5)
        ),
        fixedRow(
          numericInput("n", "Sample Size", value = 50)
        )
      ),
      conditionalPanel(
        condition = "input.test == 'Chi Square'",
        fixedRow(
          column(6,
            numericInput("dfc", "DF", value = 1, min = 1)
          )
        )
      ),
      conditionalPanel(
        condition = "input.test == 'F'",
        fixedRow(
          column(6,
            numericInput("df1", "DF1", value = 1, min = 1)
          ),
          column(6,
            numericInput("df2", "DF2", value = 1, min = 1)
          )
        )
      ),
      width = 3
    ),


# Main Panel Design -------------------------------------------------------

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      # helpText('An irrational number \\(\\sqrt{2}\\)
      #      and a fraction $$1-\\frac{1}{2}$$'),
      # helpText('and a fact about \\(\\pi\\):
      #      $$\\frac2\\pi = \\frac{\\sqrt2}2 \\cdot
      #      \\frac{\\sqrt{2+\\sqrt2}}2 \\cdot
      #      \\frac{\\sqrt{2+\\sqrt{2+\\sqrt2}}}2 \\cdots$$'),
      fixedRow(
        column(6,
          uiOutput('distribution'),
          uiOutput('transform')
        ),
        column(6,
          uiOutput('pvalue_stat'),
          uiOutput('pvalue_z')
        )
      )
    )
  )
)
