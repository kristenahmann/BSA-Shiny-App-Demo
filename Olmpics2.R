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
olympics$sex <- as.factor(olympics$sex)
# add in total number of medals
olympics <- olympics %>% mutate(total_medals = gold+silver+bronze)




#good
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Rio Olympics Analysis"),
    
    sidebarLayout(
        sidebarPanel(
            # create dropdown menu
            selectInput(
                
                "sport",
                "Sport",
                levels(olympics$sport),
                multiple = FALSE # change if want to show multiple classes
            ),
            selectInput(
                "sex",
                "Sex",
                levels(olympics$sex),
                multiple = FALSE
            )
        ),
        mainPanel(
            plotOutput("distPlot"),
            verbatimTextOutput("extra_info")
        )
        
    )
    
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        
        dat <- olympics %>% filter(sport == input$sport) %>% filter(sex == input$sex) 
        dat$total_medals <- as.factor(dat$total_medals)
        ggplot(dat, aes(x = height, y = weight)) + 
            geom_point(aes(color = total_medals)) +
            geom_smooth(method = "lm") +
            labs(title ="Athlete Height & Weight")
        
            
    })
    output$extra_info <- renderPrint({
        dat_2 <- olympics %>% filter(sport == input$sport) %>% filter(sex == input$sex) %>% filter(total_medals > 0 )
        dat_2 <- dat_2 %>% select(name, nationality, total_medals)
        dat_2
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
