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
      fixedRow(
        column(
          width = 8,
          selectInput(
            "test", "Type of Test:",
            choices = c(
              "One Sample Proportion",
              "One Sample Mean",
              "Two Samples Mean"
            ), selected = 'One Sample Mean'
          )
        ),
        column(
          width = 4,
          numericInput("alpha", "α", value = 0.05, min = 0, max = 1, step = 0.01)
        )
      ),
      fixedRow(
        column(
          width = 6,
          radioButtons(
            'alternative', 'Alternative Hypothesis:',
            choices = c("<", ">", "≠"),
            inline = TRUE
          )
        ),
        column(
          width = 6,
          numericInput("n", "Sample Size", value = 25)
        )
      ),

# One Sample Proportion ---------------------------------------------------
      conditionalPanel(
        condition = "input.test == 'One Sample Proportion'",
        fixedRow(
          column(
            width = 6,
            numericInput(
              "p", "Null Proportion:",
              value = 0.5, min = 0, max = 1, step = 0.01
            )
          ),
          column(
            width = 6,
            numericInput(
              "phat", "Sample Proportion",
              value = 0.5, min = 0, max = 1, step = 0.01
            )
          )
        )
      ),

# One Sample Mean ---------------------------------------------------------
      conditionalPanel(
        condition = "input.test == 'One Sample Mean'",
        fixedRow(
          column(
            width = 6,
            numericInput("mu", "Null Mean", value = 0)
          ),
          column(
            width = 6,
            numericInput("xbar", "Sample Mean", value = 0)
          )
        ),
        fixedRow(
          column(
            width = 6,
            switchInput(
              inputId = 'pop_std', label = 'Standard Deviation', value = TRUE,
              onLabel = 'Population', offLabel = 'Sample', onStatus = 'success',
              offStatus = 'danger', inline = TRUE
            )
          ),
          column(
            width = 6,
            numericInput("sig", 'Standard Deviation', value = 1, min = 0)
          ),
        )
      ),
      actionButton("update" ,"Update View", icon("refresh"),
        class = "btn btn-primary"),
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
