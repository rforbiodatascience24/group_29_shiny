library("shiny")
library("bslib")
library("ggplot2")
library("stringr")
# Define the User Interface (Frontend)
ui <- page_fluid(
  layout_columns(
    col_widths = 12,
    card(
      titlePanel("Virtual Central Dogma"),
      style = "background-color: #f0f0f0; padding: 15px;"
    )),
  layout_columns(
    col_widths = 12,
    card(
      titlePanel("About"),
      helpText("Generate an example gene sequence of a desired length with desired probabilities for each base")
    )),
  layout_columns(
    col_widths = 12,
    card(
      card_header("Gene input variables"),
      sliderInput(inputId = "n_bases",
                  label = "Number of bases:",
                  min = 1,
                  max = 60,
                  value = 30,
                  width = "100%"),
      layout_columns(
        colwidths = c(3,3,3,3),
        numericInput(inputId = "A_prob",
                     label = "Probability of A",
                     value = 0.25,
                     min = 0,
                     max = 1),
        numericInput(inputId = "T_prob",
                     label = "Probability of T",
                     value = 0.25,
                     min = 0,
                     max = 1),
        numericInput(inputId = "C_prob",
                     label = "Probability of C",
                     value = 0.25,
                     min = 0,
                     max = 1),
        numericInput(inputId = "G_prob",
                     label = "Probability of G",
                     value = 0.25,
                     min = 0,
                     max = 1)
      ))),
  layout_columns(
    col_widths = 12,
    card(
      card_header("Virtual DNA Gene output"),
      mainPanel(
        verbatimTextOutput(outputId = "dna")
      )
    )),
  layout_columns(
    col_widths = 12,
    card(
      card_header("Virtual RNA transcript output"),
      mainPanel(
        verbatimTextOutput(outputId = "rna")
      )
    )),
  layout_columns(
    col_widths = 12,
    card(
      card_header("Virtual Amino Acid sequence output"),
      mainPanel(
        verbatimTextOutput(outputId = "protein")
      )
    )),
  layout_columns(
    col_widths = 12,
    card(
      card_header("Plot of base counts"),
      mainPanel(
        tableOutput(outputId = "freqcount")
      )
    )),
  layout_columns(
    col_widths = 12,
    card(
      card_header("Virtual Amino acid frequency plot output"),
      mainPanel(
        plotOutput(outputId = "freqplot")
      )
    ))
)
