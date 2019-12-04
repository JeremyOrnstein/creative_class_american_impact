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
library(gt)
library(broom)

creative_people_income <- read_rds("creative_people_income.rds")

x_choices <- c("County" = "County.x", 
               "Metro" = "metro03", 
               "Share of Creative Class" = "Creative2000S", 
               "Per Capita Income" = "PerCapitaInc", 
               "Migration Rate" = "NetMigrationRate1018")
y_choices <- c("Share of Creative Class" = "Creative2000S",
               "Change in Creative Class" = "difference",
               "Per Capita Income" = "PerCapitaInc")



regression <- creative_people_income %>%  
  lm(formula = Creative2000S ~ metro03 + PerCapitaInc) 
table_r <- tidy(regression) %>%
  mutate(term = recode(term, 
         '(Intercept)' = 'Intercept', 
         'metro03' = 'Metro',
         'PerCapitaInc' = 'Income',
         'metro03:PerCapitaInc' = 'Both')) %>% 
  select('Term' = term,
         'Estimate' = estimate,
         'Standard Error' = std.error,
         'Fit' = statistic,
         'P-Value' = p.value)

topS_25_raw <- creative_people_income %>% 
  arrange(desc(Creative2000S)) %>% 
  select(Creative2000S, County.x, metro03, PerCapitaInc) %>%
  head(25)

topS_25 <- topS_25_raw %>% 
  select('Share of Creative' = 'Creative2000S',
         'County' = 'County.x',
         'Metro' = 'metro03',
         'Income' = 'PerCapitaInc')



ui <- fluidPage(
  navbarPage(
  "Impact of the American Creative Class",
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
      "Regression Model",
      gt_output("Table_Model"),
      br(),
      h4("Analysis")
    ))
    
    ), 
  
  tabPanel(
    "Story",
    gt_output("Story"),
    br(),
    h4("Analysis")
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
            labs(title = "Relationship",
                 caption = "Each graph shows the relationship between population, income, and creative class. 
                 The steeper the blue line-- or line of best fit-- the greater the relationship. 
                 Notice how 'metro' causes a single change, because the county can be only metro or not, 
                 while 'income' has a wide range. Notice how there are strong relationships in regards to 'Share', 
                 which is the measure of creative class at one time, relative to the relationships in regards to 'Change', 
                 which is the measure of creative class over time.") 
    })
  
  output$Story <- render_gt({
    topS_25
})
  
  output$Table_Model <- render_gt({
    table_r
  })
  
  output$Text <- renderText({"I'm Jeremy Ornstein, a first year student at Harvard University. I'm a poet, 
  an actor, an R coder-- I'm creative! But I also recognize the need for 
  This data comes from studies done by the USDA Economic Research Service, 
  and the American Community Survey. 
  Our world isn't perfect, and we need more people who can imagine how to make things better. 
  This data represents how creative people relate to places, both their population density and income."
  })
}


# Run the application
shinyApp(ui = ui, server = server)
