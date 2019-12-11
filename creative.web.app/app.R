library(readxl)
library(sf)
library(ggplot2)
library(leaflet)
library(shiny)
library(gt)
library(broom)
library(shinythemes)
library(tidyverse)

creative_people_income <- read_rds("test.rds")

creative_people_income_new <- read_rds("test.rds")

# I read in my rds file. 

x_choices <- c("County" = "County.x", 
               "Metro" = "metro_00", 
               "Share of Creative Class" = "Creative2000S", 
               "Migration Rate" = "NetMigrationRate1018",
               "Per Capita Income" = "PerCapitaInc")

y_choices <- c( "Share of Creative Class" = "Creative2000S", 
               "Per Capita Income" = "PerCapitaInc")

# Labelling these choices allowed me to neaten up my graphs later. 

regression <- creative_people_income %>%  
    lm(formula = Creative2000S ~ metro_00 + PerCapitaInc) 
table_r <- tidy(regression) %>%
    mutate(term = recode(term, 
                         '(Intercept)' = 'Intercept', 
                         'metro_00' = 'Metro',
                         'PerCapitaInc' = 'Income',
                         'metro_00:PerCapitaInc' = 'Both')) %>% 
    select('Term' = term,
           'Estimate' = estimate,
           'Standard Error' = std.error,
           'Fit' = statistic,
           'P-Value' = p.value)

# I used lm to find the impact of metro and income on creative share, tidied the regression so I could use the numbers, and neatened the columns. 

topS_25_raw <- creative_people_income %>% 
    arrange(desc(Creative2000S)) %>% 
    select(Creative2000S, County.x, metro_00, PerCapitaInc, TotalPopEst2018) %>%
    head(25)

# I arranged by creative and selected the first 25 rows with columns I wanted, to show in a table later 
# I neatened the names of the columns for the table below 

topS_25 <- topS_25_raw %>% 
    select('Share of Creative' = 'Creative2000S',
           'County' = 'County.x',
           'Metro' = 'metro_00')

map_values2 <- creative_people_income_new %>% 
  arrange(desc(Creative2000N)) %>% 
  head(100) %>% 
  select(Long, Lat)

Map <- leaflet(data = map_values2) %>% 
  addProviderTiles("Stamen.Toner", group = "Toner") %>% 
  addMarkers(~Long, ~Lat)


# Below, I began to build my page with a tabpanel that contained graphs, a page where users can manipulate a sidebar to 
# See the relationship between different variables, which are shown in plots. I also included a table that models the relationship 
# In a few numbers, using the regression code from up above. Below the table I wrote several paragraphs to describe the table. 

ui <- fluidPage(theme = shinytheme("superhero"),
    navbarPage(
        "Impact of the American Creative Class",
        # This is the next panel, which summarizes the findings of the entire page in writing. 
        tabPanel(
          "Summary",
          h2("Summary"),
          h4("Richard Florida led the argument for the importance of the creative class. We define the creative class as those occupations 
    in which workers must think up new ideas or things, including artistic projects. We exlude jobs like teachers, doctors, judges-- although these occupations 
    require creative thinking, they are proportional to the population. Instead, we are measuring all of the extra creative work: people who are spending surplus 
    energy to build a new world, slowly but surely."),
    br(),
    h4("The USDA Economic Research Service tested Florida's thesis, and agreed on the conclusions: 
    the creative class of Americans tends to live in urban counties, and counties that correspond with high incomes.
    The data trends shown in the graph and regression model affirm these claims. As income goes up, creative class goes up. If a county is a metro area, creative 
    class goes up. Migration into a county corresponds with higher creative class, as well. Finally, in counties with higher income, creative class has increased faster."),
    br(),
    h4("Affirmed by the Story tab, and included in the Florida thesis, is the claim that the creative class concentrates in places of natural beauty, 
    even if those places are not urban. Noted in the Story tab but unmentioned in the ERS thesis is the impact of government investment on creative class; 
    Los Alamos and counties close to D.C demonstrate that military investment draws high numbers of members of the creative class."),
    br(),
    h4("The Map tab reveals just how concentrated are the members of the creative class, clustering around big cities along the coasts. Although several high-creative counties 
       are closer to the heartland, these are also lower on the list."),
    br(),
    h4("The better we can understand why the creative class concentrates here or there, 
       the better we can reckon with how to develop creative capacity in every corner of the nation.")
        ),
        
        #tabsetPanel(type = "tabs", 
        tabPanel(
            "Relationships",
            tabsetPanel(
                tabPanel(
                    "Graphs",
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
                            ),
                            h5("Each graph shows the relationship between population, income, and creative class. 
                               The steeper the blue line-- or line of best fit-- the greater the relationship. 
                               Notice how 'metro' causes a single change, because the county can be only metro or not, 
                               while 'income' has a wide range.")
                        )
                    )
                ),
                
                tabPanel(
                    "Regression Model",
                    gt_output("Table_Model"),
                    br(),
                    h4("This table shows the impact of two factors on the creative share of a county: whether or not a county is urban, and the per capita income of a county.
        If the P-Value were higher than 0.05, it'd show a 'null hypothesis': the factor would have little to no influence on the share of creative class. 
         But both of these factors have p-values far below 0.05, so they have a clear impact. The standard of error is low, and the fit is high, confirming 
         that the relationship is strong. The final column, estimate, shows the specific impact of the factor on the creative share."),
                    br(),
                    h4("These numbers are all very low, because only a slim portion of a population belongs to the creative class. So relative to those numbers,
         whether or not a county is metro has a big impact on its share of creative class. Metro is a 1,2 choice, so either a county qualifies or not. Income
         is a broader range, so it takes more of an increase in income to correspond with more creative class. But as seen from the P-Value, income, too, 
         has a very significant relationship with a county's share of the creative class."
                    ))
                
            )), 
        # This is the next panel, which turns a tibble from up above into a gt, to tell a particular story about 
        # Where creative class is concentrated. 
        tabPanel(
            "Story",
            h2("Top 25 Counties by Share of Creative Class"),
            gt_output("Story"),
            br(),
            h4("This table shows the top 25 counties by their share of creative class. In hard numbers rather than share, the top 100 counties hold 
       just over 16 million members of the creative class-- which is more than half of the total creative class in our nation! In other words,
       members of the creative class are concentrated in a few key areas. But this table shows the counties in which 
       creative people are especially concentrated. Middlesex County, my home, just makes the cut-- probably for its artists and biotech workers.
       Several of the counties listed are in Virginia and Maryland: perhaps engineers and thinkers for the U.S Government are concentrated in those counties.
       Except for two, all of the counties listed are measured as metro areas, so it stands to reason that most of these places are also packed urban areas."),
            br(),
            h4("Pitkin CO is one of the non-metro counties on the list.
    It's representative of several other unexpected data points farther down the high-creative list, because of its natural beauty. 
      Pitkin County isn't a bustling metropolis; quite the opposite. But it IS full of lakes, cliffs, and wild places that draw creative people. 
      Natural beauty is a shared quality among the non-metro counties that still score high on the creative class list.."),
            br(),
            h4("Why does Los Alamos top the list? It's one of the least populous counties in New Mexico! How do we explain its high share of creative class?
       It turns out that the community of Los Alamos was created by the U.S military to build an atom bomb. It stands to reason 
       that a community created for the purpose of top-secret engineers would have a very high proportion of engineers. And who could 
       be called more creative than the minds who introduced to society the tremendous and terrifying power of nuclear energy?")
        ),
        
        tabPanel(
          "Map",
          leafletOutput("Map"),
          br(),
          h4("The top 100 counties by creative class, in numbers rather than share, are shown on the map. Observe how they are centered around major cities, clustered on the coasts.
          These counties together hold 16 million members of the creative class. 
             And, there are half as many members of the creative class in these 100 counties, as there are in the 
             rest of the 3,000 counties combined! In other words-- the American creative class is highly concentrated.")
        ),
        
        # This is the final panel, which offers information about me and the data sources. 
        
        tabPanel(
            "About",
            textOutput("Text")
        )
    ))

# This is the server, where I write the code that fills in the tab panels from up above
# The first panel writes the code for a simple plot that takes variables from up above, and draws a line of best fit through. 
# My caption is univrsal for each plot. 
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
    
    output$Story <- render_gt({
        topS_25
    })
    
    output$Table_Model <- render_gt({
        table_r
    })
    
    output$Map <- renderLeaflet({
      Map
    })
    
    output$Text <- renderText({"I'm Jeremy Ornstein, a first year student at Harvard University. I'm a poet, 
  an actor, an R coder-- I'm creative! But I'm also a citizen, and my mind and heart are on the question of the climate crisis.
  How do we transform our society to decarbonize, while also weave social connections between one another? I'm excited to think about 
  how we can build our capacity of artists and engineers in EVERY corner of our nation, to take on these big questions to create a better world. 
  This data comes from studies done by the USDA Economic Research Service, and the American Community Survey. It's inspired by the work of Richard Florida 
  on the American creative class. You can reach me at jornstein@college.harvard.edu."
    })
}

# My other rendered lines above, take code from the top of my page, feed it into a Shiny function, and apply it to a tab panel.
# I can write out the text for two of them, and simply write the name of two of the other tibbles. 

# Run the application
shinyApp(ui = ui, server = server)
