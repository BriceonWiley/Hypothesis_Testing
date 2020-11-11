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
        "test", "Type of Distribution:",
        choices = c("Normal", "t", "Chi Square", "F"),
        # width = "50%"
      ),
      conditionalPanel(
        condition = "input.test == 'Normal'",
        fixedRow(
          column(6,
            numericInput("mu", "Mean", value = 0)
          ),
          column(6,
            numericInput("sig", "SD", value = 1, min = 1)
          )
        )
      ),
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
