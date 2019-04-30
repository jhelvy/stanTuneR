library(rstan)
# Set auto_write to false because I want to always search from scratch
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())

server <- function(input, output) {

  output$selected_distribution <- renderText({
    paste('You have selected a ', input$distribution,
          ' distribution with the following properties:',
          sep='')
  })

  output$selected_boundaries <- renderUI(HTML(
    paste('<ul>',
      '<li>', 100*input$dens[1], '% of the density will be below ',
      input$bound_L, '</li>',
      '<li>', 100*(1 - input$dens[2]), '% of the density will be above ',
      input$bound_U, sep=''), '</li>',
      '<li>', 100*(input$dens[2] - input$dens[1]),
      '% of the density will be between ', input$bound_L, ' and ',
      input$bound_U, '</li>',
      '</ul>', sep=''))

}



