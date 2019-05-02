# ----------------------------------------------------------------------------
# Initial setup

# Load libraries
library(shiny)
library(rstan)

# Stan settings
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load the functions
funcs <- new.env()
source('functions.R', local=funcs)

# ----------------------------------------------------------------------------
# Functions for Shiny app

getTargetsHtml <- function(input) {
  targetsHtml <- paste(
    '<p>Press the "Tune It!" button to search for the parameters of a <b>',
    input$distribution, '</b> distribution with the following properties:</p>',
    '<ul>',
    '<li>', 100*input$dens[1], '% of the density below ',
    input$bound_L, '</li>',
    '<li>', 100*(1-input$dens[2]), '% of the density above ',
    input$bound_U, '</li>',
    '<li>', 100*(input$dens[2] - input$dens[1]),
    '% of the density between ', input$bound_L, ' and ',
    input$bound_U, '</li>', '</ul>', sep='')
  return(targetsHtml)
}

getRenamedDistribution <- function(input) {
  df = data.frame(
        name      = c('Normal', 'Beta', 'Inverse-Gamma'),
        codedName = c('normal', 'beta', 'inv_gamma'))
  return(as.character(df[which(df$name == input$distribution),]$codedName))
}

getTargets <- function(input) {
  targets = list(
    bound_L = input$bound_L,      # LOWER quantile boundary
    bound_U = input$bound_U,      # UPPER quantile boundary
    dens_L  = input$dens[1],      # Target density below bound_L
    dens_U  = 1 - input$dens[2])  # Target density above bound_U
  return(targets)
}

getParamsOutput <- function(result) {
  params = as.character(result$params)
}

# ----------------------------------------------------------------------------
# Define UI

ui <- fluidPage(

  # App title ----
  titlePanel('Stan Tuner'),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for choosing dataset ----
      selectInput(inputId = 'distribution',
                  label = 'Select a distribution:',
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
                  min = 0, max = 1, value = c(0.01, 0.99)),

      actionButton('tuneIt', 'Tune It!')

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Verbatim text for data summary ----
      uiOutput('selected_targets'),
      textOutput('params')

    )
  )
)

# ----------------------------------------------------------------------------
# Define Server

server <- function(input, output) {

  output$selected_targets <- renderUI(HTML(getTargetsHtml(input)))

  observeEvent(input$tuneIt, {
    distribution  <- getRenamedDistribution(input)
    targets       <- getTargets(input)
    result        <- funcs$tuneParams(distribution, targets)
    output$params <- renderText(getParamsOutput(result))
    }
  )

}

# ----------------------------------------------------------------------------
# Run the app

shinyApp(ui, server)

