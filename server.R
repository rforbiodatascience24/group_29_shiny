library("shiny")
library("ggplot2")
library("stringr")
source(file = "app_functions.R")

#Define the Server (Backend)
server <- function(input, output) {
  wrapper_output <- reactive({
    wrapper(length = input$n_bases,
            prob = c(input$A_prob,input$T_prob,input$C_prob,input$G_prob))
  })
  output$dna <- renderText({wrapper_output()$DNA})
  output$rna <- renderText({wrapper_output()$RNA})
  output$protein <- renderText({wrapper_output()$AA_seq})
  output$freqplot <- renderPlot({wrapper_output()$PLOT})
}
