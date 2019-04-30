# Define UI for dataset viewer app ----
ui <- fluidPage(

  # App title ----
  titlePanel('Stan Tuner'),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for choosing dataset ----
      selectInput(inputId = 'distribution',
                  label = 'Choose a distribution:',
                  choices = c('Normal', 'Beta', 'Inverse-Gamma')),

      # Input: Numeric entry for LOWER quantile boundary ----
      numericInput(inputId = 'bound_L',
                   label = 'Lower quantile boundary:',
                   value = -2),

      # Input: Numeric entry for UPPER quantile boundary ----
      numericInput(inputId = 'bound_U',
                   label = 'Upper quantile boundary:',
                   value = 2),

      sliderInput('dens',
                  label = 'Target density limits:',
                  min = 0, max = 1, value = c(0.01, 0.99))

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Verbatim text for data summary ----
      textOutput('selected_distribution'),
      uiOutput('selected_boundaries')

    )
  )
)
