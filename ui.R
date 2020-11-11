# Cmd + Shift + Enter to run App

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Hyptothesis Testing"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "test", "Type of Distribution:",
        choices = c("Normal", "t", "χ^2", "F")
      ),
      conditionalPanel(
        condition = "input.test == 'Normal'" ,
        textInput(
          "mu", "Mean", value = 0, width = "20%"
        ),
        textInput(
          "sig", "Standard Deviation", value = 1, width = "20%"
        )
      ),
      conditionalPanel(
        condition = "input.test == 't'",
        textInput(
          "df", "Degrees of Freedom", value = 1, width = "20%"
        )
      ),
      conditionalPanel(
        condition = "input.test == 'χ^2'",
        textInput(
          "df", "Degrees of Freedom", value = 1, width = "20%"
        )
      ),
      conditionalPanel(
        condition = "input.test == 'F'",
        textInput(
          "df1", "First Degrees of Freedom", value = 1, width = "20%"
        ),
        textInput(
          "df2", "Second Degrees of Freedom", value = 1, width = "20%"
        )
      )
    ),


    # Show a plot of the generated distribution
    mainPanel(
      # plotOutput("distPlot")
    )
  )
)
