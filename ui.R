# Cmd + Shift + Enter to run App

library(shiny)
library(ggplot2)

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
        ),
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
          column(6,
            numericInput("p", "Hypothesized Proportion", value = 0.5)
          ),
        ),
        fixedRow(
          column(6,
            selectInput(
              "hypot", "Type of Hypothesis:",
              choices = c("<=", ">=", "!=")
            )
          ),
          column(6,
            numericInput("alpha", "α", value = 0.05, min = 0, max = 1, step = 0.01)
          ),
        ),
        fixedRow(
          column(6,
            numericInput("phat", "Sample Proportion", value = 0.5)
          ),
          column(6,
            numericInput("n", "Sample Size", value = 50)
          )
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
        condition = "input.test == 't'",
        fixedRow(
          column(6,
            numericInput("dft", "DF", value = 1, min = 1)
          )
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
      width = 2
    ),


# Main Panel Design -------------------------------------------------------

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
