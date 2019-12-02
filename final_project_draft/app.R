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
library(shiny)

creative_people_income <- read_rds("creative_people_income.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Impact of the Creative Class"),

 
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("income"),
           plotOutput("income_change"),
           plotOutput("migration"),
           plotOutput("metro"),
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        change <- creative_people_income %>% mutate(difference = Creative2000N - Creative2000S)
        change %>% ggplot(aes(PopDensity2010, difference)) + geom_point() + geom_smooth(method = "lm", se = TRUE) + 
            xlab("Population Density") + ylab("Change in Creative Class 1990-2000, Share of Pop") + scale_y_log10() + scale_x_log10() + labs(title = "Relationship between Population Density and Creative Class", subtitle = "Coefficient of PopDensity = 27.89")
    })
    
    output$income <- renderPlot({
        
        creative_people_income %>% 
            ggplot((aes(x = PerCapitaInc, Creative2000S))) + geom_point() + geom_smooth(method = "lm", se = TRUE) + xlab("Per Capita Income") + ylab("Creative Class, Share of Pop") + labs(title = "Relationship between Per Capita Income and Creative Class")
        
    })
    
    output$income_change <- renderPlot({
        
        change %>% ggplot(aes(PerCapitaInc, difference)) + geom_point() + geom_smooth(method = "lm", se = TRUE) +
            xlab("Per Capita Income") + ylab("Change in Creative Class 1990-2000, Share of Pop") + scale_y_log10() + scale_x_log10() + labs(title = "Relationship between Per Capita Income and Change in Creative Class")
        
    })
    
    output$migration <- renderPlot({
        
        creative_people %>%
            ggplot((aes(x = NetMigrationRate1018, Bohemian2000S))) + geom_point() + geom_smooth(method = "lm", se = TRUE) +xlab(" Net Migration Rate") + ylab("Bohemians, Share of Pop") + labs(title = "Relationship between Migration and Bohemians")        
    })
    
    output$metro <- renderPlot({
        
        creative_people_income %>% ggplot(aes(x = `metro 1993 definition (1=metro, 0=nonmetro)`, Creative2000S)) + geom_jitter() + xlab("Metro Status") + ylab("Creative Class") + labs(title = "Relationship between Metro Status and Creative Class")
        
    })
        }

# Run the application 
shinyApp(ui = ui, server = server)
