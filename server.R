library(rstan)
# Set auto_write to false because I want to always search from scratch
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())

server <- function(input, output) {

  output$selected_distribution <- renderText({
    paste('You have selected the ', input$distribution, ' distribution',
          sep='')
  })
  output$selected_boundaries <- renderText({
    paste('You have set the boundaries such that:\n',
          100*input$dens[1], '% of the density will be below ', input$bound_L,
          '\n',
          100*input$dens[2], '% of the density will be above ', input$bound_U,
          sep='')
  })

  output$selected_boundaries <- renderUI(HTML(
    paste('<p>You have set the boundaries such that:<br></p>',
    paste('<ul><li>', 100*input$dens[1], '% of the density will be below ',
          input$bound_L, '</li>', sep=''),
    paste('<li>', 100*input$dens[2], '% of the density will be above ',
          input$bound_U, sep=''), '</li></ul>', sep='')))

}



