library(shiny)

# Define UI for application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Tidal height and metabolism correlations as a function of half-window widths"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
  
    selectInput("site", 
        label = h3("Select site"), 
        choices = list(
          "Elkhorn Slough" = "ELKVM",
          "Padilla Bay" = "PDBBY",
          "Rookery Bay" = "RKBMB",
          "Sapelo Island" = "SAPDC"
          ),
          selected = "ELKVM"),

    sliderInput("months", "Select months",
        min = 1, max = 12, step = 1, value = c(1, 12)
      ),
    
    radioButtons('day', label = h3('Day'),
      choices = list("one" = 1, "three" = 3, "six" = 6, "nine" = 9, "twelve" = 12), selected = 1),
    
    radioButtons('hour', label = h3('Hour'),
      choices = list("one" = 1, "three" = 3, "six" = 6, "nine" = 9, "twelve" = 12), selected = 1),
    
    radioButtons('tide', label = h3('Tidal range'), 
      choices = list("0.2" = 0.2, "0.4" = 0.4, "0.6" = 0.6, "0.8" = 0.8, "1" = 1), selected = 0.2),
    
  width = 3
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Correlation plots", plotOutput("corrplot", height = "110%")),
      tabPanel("Metabolism plots", plotOutput("metabplot", height = "110%")),
      tabPanel("Summary tables", h3('Summary of metabolism estimates'), tableOutput("tablemet"), h3('Correlations with tidal change'), tableOutput('tablecorr'))
      ), width = 9
    )
    
))