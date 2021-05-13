# packages
library(shiny)
library(tidyverse)
library(DT)

# read in data
data <- read.csv("athletes.csv")

# remove first column that has IDs
olympics <- data[,-1]

# convert sport and nationality to factors
olympics$sport <- as.factor(olympics$sport)
olympics$nationality <- as.factor(olympics$nationality)

# add in total number of medals
olympics <- olympics %>% mutate(total_medals = gold+silver+bronze)


# Define UI for application that draws a histogram
ui_olympic <- fluidPage(

    # Application title
    titlePanel("Rio Olympics Athlete Analysis"),
  
    sidebarLayout(
        sidebarPanel(
            
            selectInput(
                "sport_type",
                "Select Sport",
                levels(olympics$sport),
            ),
            
            selectInput(
                "country",
                "Select Country",
                levels(olympics$nationality),
            ),
            
            sliderInput(
                "medals",
                label = "Number of Total Medals",
                min = 0, max =6, value = 0
            )

        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Athletes",DT::dataTableOutput("athlete_tbl")),
                tabPanel("Plot")
            )
        )
      
    )
)

# Define server logic 
server_olympic <- function(input, output){

    output$athlete_tbl <- DT::renderDataTable({
        
        DT::datatable(olympics %>% 
                          filter(sport == input$sport_type) %>% 
                          filter(nationality == input$country) %>% 
                          filter(total_medals == input$medals) )
    })
}

# Run the application 
shinyApp(ui = ui_olympic, server = server_olympic)
