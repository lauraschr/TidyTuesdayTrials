#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)


nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
nobel_winner_all_pubs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Nobel Prize Winners"),
   
   # Sidebar with a slider input for number of bins
   tabsetPanel(
      tabPanel(
        "Nobel Prize Winners",
        sidebarLayout(
          sidebarPanel(
            selectInput("xachse", "Select the Variable for the x-axis", choices = names(nobel_winners)),
            sliderInput("select_year", label="Select a year", 
                        min = min(nobel_winners$prize_year),
                        max = max(nobel_winners$prize_year),
                        value = 1980)
                        
          ),
          
          mainPanel(wellPanel(
            plotOutput("mainplot")
          
          )
        )
          )
      )
   )
)
        
     
          



# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$mainplot <- renderPlot({
     nobel_winners %>%
       filter(prize_year == input$select_year) %>% 
       ggplot() + aes_string(x = input$xachse, y = "gender") + 
       ylab ("Geschlecht") +
       geom_point() + 
       coord_flip()
     
      
   
   })
   
 
}

# Run the application 
shinyApp(ui = ui, server = server)

