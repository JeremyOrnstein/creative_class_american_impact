#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(readxl)
library(sf)
library(ggplot2)
library(leaflet)
library(shiny)

creative_people_income <- read_rds("creative_people_income.rds")

x_choices <- c("County" = "County.x", 
               "Metro" = "metro03", 
               "Share of Creative Class" = "Creative2000S", 
               "Per Capita Income" = "PerCapitaInc", 
               "Migration Rate" = "NetMigrationRate1018")
y_choices <- c("Share of Creative Class" = "Creative2000S",
               "Change in Creative Class" = "difference")

ui <- fluidPage(
  navbarPage(
  "Jeremy's Pset",
  tabPanel(
    "Map",
      leafletOutput("Map")
    ),
  #tabsetPanel(type = "tabs", 
  tabPanel(
    "Graphs",
    tabsetPanel(
    tabPanel(
      "Relationships",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "x",
            label = "x variable?",
            choices = x_choices
          ),
          selectInput(
            inputId = "y",
            label = "y variable?",
            choices = y_choices
          )
        ),
        mainPanel(
          plotOutput(
            "Plot"
          )
        )
      )
    ),
    
    tabPanel(
      "Model"
    ))
    
    ), 

  # I made a title for my map page and inserted a placeholder for my Plot

  tabPanel(
    "About",
      textOutput("Text")
    )
  )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Plot <- renderPlot({
        creative_people_income %>%
            ggplot((aes(x = get(input$x), y = get(input$y)))) + 
            geom_jitter() + 
            geom_smooth(method = "lm", se = TRUE) + 
            xlab("X Variable") + 
            ylab("Y Variable") + 
            labs(title = "Relationship")
    })
  
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(dragging = TRUE)) %>%
                   addProviderTiles() %>% 
                   addMarkers(lng = map$lng, lat = map$lat, popup = map$County.x)
})
  
  output$Text <- renderText({"This data comes from studies done to find population and income, 
  stored in the Rural Atlas and other databases. 
  Our world isn't perfect, and we need more people who can imagine how to make things better. 
  This data represents how creative people relate to places, both their population density and income."
  })
}


# Run the application
shinyApp(ui = ui, server = server)
