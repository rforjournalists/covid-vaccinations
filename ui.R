
library(tidyverse)
library(sf)
library(shiny)
#devtools::install_github("yutannihilation/ggsflabel")
#library(ggsflabel)

vacc <- read_csv('vaccinations.csv')



fluidPage(    
  
  # Give the page a title
  titlePanel("COVID-19 vaccinations in England"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    sidebarPanel(
      # Define the sidebar with one input
      selectizeInput('LA', 'Choose a local authority:', 
                     choices = c("Your authority", sort(unique(vacc$LA_name))), 
                     multiple = FALSE, 
                     options = list(maxItems = 1))
      

      
    ),
    # Create a spot for the barplot
    mainPanel(
      plotOutput("vaccinePlotLA", width = '100%', height = '500px')
    )
    
  )
)