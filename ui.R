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
      fluidRow(
        column(
          width = 8,
          selectInput(
            "test", "Type of Test:",
            choices = c(
              "One Proportion",
              "One Mean",
              "Dependent Samples",
              "Independent Samples",
              "Two Proportions"
            ), selected = 'Dependent Samples'
          )
        ),
        column(
          width = 4,
          numericInput("alpha", "α", value = 0.05, min = 0, max = 1, step = 0.01)
        )
      ),
      fluidRow(
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
        condition = "input.test == 'One Proportion'",
        fluidRow(
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
        condition = "input.test == 'One Mean'",
        fluidRow(
          column(
            width = 6,
            numericInput("mu", "Null Mean", value = 0)
          ),
          column(
            width = 6,
            numericInput("xbar", "Sample Mean", value = 0)
          )
        ),
        h5('Standard Deviation:'),
        fluidRow(
          column(
            width = 6,
            numericInput("sig",label = NULL, value = 1, min = 0)
          ),
          column(
            width = 6,
            selectInput(
              'std_src', NULL, c('Population', 'Sample'), 'Population'
            )
          )
        )
      ),

# Dependent Samples -------------------------------------------------------
      conditionalPanel(
        condition = "input.test == 'Dependent Samples'",
        fluidRow(
          column(
            width = 4,
            numericInput("D0", "Null Difference", value = 0)
          ),
          column(
            width = 4,
            numericInput("dbar", "Sample Difference", value = 0)
          ),
          column(
            width = 4,
            numericInput(
              "sigd", label = 'Standard Deviation', value = 1, min = 0
            )
          )
        )
      ),

# Independent Samples -----------------------------------------------------


# Two Proportions ---------------------------------------------------------



      fluidRow(
          actionButton(
            "update" ,"Update", icon("refresh"),
            class = "btn btn-primary"
          ),
        align = 'center'
      ),
      # width = 3
    ),


# Main Panel Design -------------------------------------------------------

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        type = 'tabs', # 'pills'
        tabPanel('Plot', plotOutput("distPlot")),
        tabPanel(
          title = 'Results',
          fluidRow(uiOutput('distribution')),
          fluidRow(uiOutput('pvalue_stat')),
          fluidRow(uiOutput('pvalue_z')),
          fluidRow(uiOutput('CI'))
        )
      ), #width = 9
    )
  )
)
