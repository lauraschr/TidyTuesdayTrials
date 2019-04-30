#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("DT")

library(shiny)
library(tidyverse)
library(DT)

#write_rds(anime, "temp.rds")
anime <- read_rds("temp.rds")

anime <- short_anime %>%  select(animeID, name, title_english, type, genre, episodes, rating, score, 
                                scored_by, rank, popularity) %>%  
  mutate(rating = factor(rating))

anime$rating
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Anime Datensatz TidyTuesday"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxInput("g", "G - All Ages", FALSE),
        checkboxInput("no", "None", FALSE),
        checkboxInput("pg13", "PG-13 - Teens 13 or older", FALSE),
        checkboxInput("pg", "PG - Children", FALSE),
        checkboxInput("r17", "R - 17+ (violence & profanity)", FALSE),
        checkboxInput("r", "R+ - Mild Nudity", FALSE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("rankingplot"),
        DT::DTOutput("tabledata")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$rankingplot <- renderPlot({
     temp <- anime %>% filter(rating == "aslödk")
     
     
     if(input$g){temp <- anime %>% filter(rating == "G - All Ages") %>% bind_rows(temp)
     
     }
     
     if(input$no){temp <- anime %>% filter(rating == "None") %>% bind_rows(temp)
     
     }
     
     if(input$pg13){temp <- anime %>% filter(rating == "PG-13 - Teens 13 or older") %>% bind_rows(temp)
     
     }
     
     if(input$pg){temp <- anime %>% filter(rating == "PG - Children") %>% bind_rows(temp)
     
     }
     
     if(input$r17){temp <- anime %>% filter(rating == "R - 17+ (violence & profanity)") %>% bind_rows(temp)
     
     }
     
     if(input$r){temp <- anime %>% filter(rating == "R+ - Mild Nudity") %>% bind_rows(temp)
     
     }
     
    temp %>% ggplot() +
       aes(x = rating, y = score) +
       geom_boxplot() +
       coord_flip() +
      labs(title = "FSK Rating unterschiedlicher Anime Filme/Serien",
          subtitle = "Boxplot der FSK-Freigabe nach Score",
          x = "FSK-Freigabe",
          y = "Bewertung")
    
  
      
   }
   )
   
   output$tabledata <- renderDT(anime)
}

# Run the application 
shinyApp(ui = ui, server = server)

