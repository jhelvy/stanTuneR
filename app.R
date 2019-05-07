# ----------------------------------------------------------------------------
# Initial setup

# Load libraries
library(dplyr)
library(shiny)
library(shinycssloaders)
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
    '<h3>Instructions:</h3>',
    '<p>Change the settings in the sidebar, then press the "Tune It!" ',
    'button to find the parameters.</p>',
    '<h3>Current settings:</h3>',
    '<b>', input$distribution, '</b> distribution with the following properties:</p>',
    '<ul>',
    '<li>', 100*input$dens[1], '% of the density below ',
    input$bound_L, '</li>',
    '<li>', 100*(1-input$dens[2]), '% of the density above ',
    input$bound_U, '</li>',
    '<li>', 100*(input$dens[2] - input$dens[1]),
    '% of the density between ', input$bound_L, ' and ',
    input$bound_U, '</li>', '</ul>', '<hr>', sep='')
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

getResultsHtml <- function(results) {
  paramsHtml    <- buildResultHtml(results$params)
  quantilesHtml <- buildResultHtml(round(results$quantiles, 5))
  resultsHtml   <- paste(
    '<h3>Results:</h3>',
    '<b>Parameter estimates:</b>',
    paramsHtml, '<br>',
    '<b>Quantile summary:</b>',
    quantilesHtml, '<br>',
    '<b>Histogram:</b>', sep='')
  return(resultsHtml)
}

buildResultHtml <- function(result) {
  html = '<p>'
  for (i in 1:length(result)) {
    html = paste(html, names(result)[i], ': ',
      result[i], '<br>', sep='')
  }
  html = paste(html, '</p>', sep='')
  return(html)
}

# ----------------------------------------------------------------------------
# Define UI

ui <- fluidPage(

  # App title
  titlePanel('Stan Tuner'),

  # Sidebar layout with a input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(

      # Input: Selector for choosing dataset
      selectInput(inputId = 'distribution',
                  label = 'Select a distribution:',
                  choices = c('Normal', 'Beta', 'Inverse-Gamma')),

      # Input: Numeric entry for LOWER quantile boundary
      numericInput(inputId = 'bound_L',
                   label = 'Lower quantile boundary:',
                   value = -2),

      # Input: Numeric entry for UPPER quantile boundary
      numericInput(inputId = 'bound_U',
                   label = 'Upper quantile boundary:',
                   value = 2),

      sliderInput('dens',
                  label = 'Target density limits:',
                  min = 0, max = 1, value = c(0.01, 0.99)),

      actionButton('tuneIt', 'Tune It!')

    ),

    # Main panel for displaying outputs
    mainPanel(

      # Output:
      htmlOutput('selected_targets'),
      htmlOutput('results') %>% withSpinner(color='#0dc5c1'),
      plotOutput('histogram', width=100)

    )
  )
)

# ----------------------------------------------------------------------------
# Define Server

server <- function(input, output, session) {

  output$selected_targets <- renderUI(HTML(
    getTargetsHtml(input)
  ))

   results <- eventReactive(input$tuneIt, {
    distribution <- getRenamedDistribution(input)
    targets      <- getTargets(input)
    return(funcs$tuneParams(distribution, targets))
  })

  output$results <- renderText({
    getResultsHtml(results())
  })

  output$histogram <- renderPlot({
    results()$histogram},
    height = 300, width = 400)

}

# ----------------------------------------------------------------------------
# Run the app

shinyApp(ui, server)
