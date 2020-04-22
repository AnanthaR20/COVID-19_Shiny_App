##### Shiny app for exploring whether we're beating COVID-19 #####

# Load required packages
require(shiny)
require(dplyr)
require(ggplot2)
require(plotly)
require(ggthemes)
require(hrbrthemes)
require(shinythemes)
require(scales)

# Load data
source("pipe3.R")

# Define UI ----
ui <- ui <- navbarPage("Are We Beating COVID-19: Is social distancing working?",
                       theme = shinytheme("flatly"),
                       
                       
                       
                       tabPanel("Visualization",
                                
                                hr(),
                                
                                sidebarLayout(
                                    
                                    sidebarPanel(
                                        helpText("Choose a region and date to see the output plot."),
                                        
                                        selectInput("region", 
                                                    label = "Choose a region to display",
                                                    choices = c("US", 
                                                                "Alabama", "Alaska", "Arizona", "Arkansas", "California",
                                                                "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",      
                                                                "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",         
                                                                "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",      
                                                                "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",     
                                                                "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                                                                "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",         
                                                                "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                                                "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                                                                "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
                                                    selected = "US"),
                                        
                                        sliderInput("date",
                                                    label = "Dates",
                                                    min = as.Date("2020-03-01","%Y-%m-%d"),
                                                    max = today()-1,
                                                    value= today()-1,
                                                    timeFormat="%Y-%m-%d")
                                    ),
                                    
                                    mainPanel(
                                        titlePanel("Growth of COVID-19"),
                                        tabsetPanel(
                                            tabPanel("New", plotOutput("region_plot")),
                                            tabPanel("Cumulative", plotOutput("region_plot_cumulative")),
                                            tabPanel("Cumulative (log10)", plotOutput("region_plot_cumulative_log"))
                                        )
                                    )
                                )
                                
                       ),
                       
                       
                       
                       tabPanel("Our Claims",
                                  
                       ),
                       
                       
                       
                       tabPanel("About",
                                
                       )
                       
)


# Define server logic ----
server <- function(input, output) {
    
    output$region_plot <- renderPlot({
        
        data <- switch(input$region, 
                       "US" = usts,
                       "Alabama" = track2("Alabama"),
                       "Alaska" = track2("Alaska"),
                       "Arizona" = track2('Arizona'),
                       "Arkansas" = track2('Arkansas'),
                       "California" = track2('California'),
                       "Colorado" = track2('Colorado'),
                       "Connecticut" = track2('Connecticut'),
                       "Delaware" = track2('Delaware'),
                       "Florida" = track2('Florida'),
                       "Georgia" = track2('Georgia'),
                       "Hawaii" = track2('Hawaii'),
                       "Idaho" = track2('Idaho'),
                       "Illinois" = track2('Illinois'),
                       "Indiana" = track2('Indiana'),
                       "Iowa" = track2('Iowa'),
                       "Kansas" = track2('Kansas'),
                       "Kentucky" = track2('Kentucky'),
                       "Louisiana" = track2('Louisiana'),
                       "Maine" = track2('Maine'),
                       "Maryland" = track2('Maryland'),
                       "Massachusetts" = track2('Massachusetts'),
                       "Michigan" = track2('Michigan'),
                       "Minnesota" = track2('Minnesota'),
                       "Mississippi" = track2('Mississippi'),
                       "Missouri" = track2('Missouri'),
                       "Montana" = track2('Montana'),
                       "Nebraska" = track2('Nebraska'),
                       "Nevada" = track2('Nevada'),
                       "New Hampshire" = track2('New Hampshire'),
                       "New Jersey" = track2('New Jersey'),
                       "New Mexico" = track2('New Mexico'),
                       "New York" = track2('New York'),
                       "North Carolina" = track2('North Carolina'),
                       "North Dakota" = track2('North Dakota'),
                       "Ohio" = track2('Ohio'),
                       "Oklahoma" = track2('Oklahoma'),
                       "Oregon" = track2('Oregon'),
                       "Pennsylvania" = track2('Pennsylvania'),
                       "Rhode Island" = track2('Rhode Island'),
                       "South Carolina" = track2('South Carolina'),
                       "South Dakota" = track2('South Dakota'),
                       "Tennessee" = track2('Tennessee'),
                       "Texas" = track2('Texas'),
                       "Utah" = track2('Utah'),
                       "Vermont" = track2('Vermont'),
                       "Virginia" = track2('Virginia'),
                       "Washington" = track2('Washington'),
                       "West Virginia" = track2('West Virginia'),
                       "Wisconsin" = track2('Wisconsin'),
                       "Wyoming" = track2('Wyoming'))
        
        data <- as_tibble(data)
        date_input <- input$date
        
        data.frame(data) %>% 
            filter(date <= date_input) %>%
            mutate(new_daily = (positive - lead(positive, n=1))) %>% 
            ggplot(aes(x = date, y = new_daily)) + 
            geom_area(fill = "#69b3a2", alpha=0.5) +
            geom_line(colour="#69b3a2") +
            theme_ipsum() +
            labs(x = "Date",
                 y = "New cases (daily)")
        
    })
    
    output$region_plot_cumulative <- renderPlot({
        
        data <- switch(input$region, 
                       "US" = usts,
                       "Alabama" = track2("Alabama"),
                       "Alaska" = track2("Alaska"),
                       "Arizona" = track2('Arizona'),
                       "Arkansas" = track2('Arkansas'),
                       "California" = track2('California'),
                       "Colorado" = track2('Colorado'),
                       "Connecticut" = track2('Connecticut'),
                       "Delaware" = track2('Delaware'),
                       "Florida" = track2('Florida'),
                       "Georgia" = track2('Georgia'),
                       "Hawaii" = track2('Hawaii'),
                       "Idaho" = track2('Idaho'),
                       "Illinois" = track2('Illinois'),
                       "Indiana" = track2('Indiana'),
                       "Iowa" = track2('Iowa'),
                       "Kansas" = track2('Kansas'),
                       "Kentucky" = track2('Kentucky'),
                       "Louisiana" = track2('Louisiana'),
                       "Maine" = track2('Maine'),
                       "Maryland" = track2('Maryland'),
                       "Massachusetts" = track2('Massachusetts'),
                       "Michigan" = track2('Michigan'),
                       "Minnesota" = track2('Minnesota'),
                       "Mississippi" = track2('Mississippi'),
                       "Missouri" = track2('Missouri'),
                       "Montana" = track2('Montana'),
                       "Nebraska" = track2('Nebraska'),
                       "Nevada" = track2('Nevada'),
                       "New Hampshire" = track2('New Hampshire'),
                       "New Jersey" = track2('New Jersey'),
                       "New Mexico" = track2('New Mexico'),
                       "New York" = track2('New York'),
                       "North Carolina" = track2('North Carolina'),
                       "North Dakota" = track2('North Dakota'),
                       "Ohio" = track2('Ohio'),
                       "Oklahoma" = track2('Oklahoma'),
                       "Oregon" = track2('Oregon'),
                       "Pennsylvania" = track2('Pennsylvania'),
                       "Rhode Island" = track2('Rhode Island'),
                       "South Carolina" = track2('South Carolina'),
                       "South Dakota" = track2('South Dakota'),
                       "Tennessee" = track2('Tennessee'),
                       "Texas" = track2('Texas'),
                       "Utah" = track2('Utah'),
                       "Vermont" = track2('Vermont'),
                       "Virginia" = track2('Virginia'),
                       "Washington" = track2('Washington'),
                       "West Virginia" = track2('West Virginia'),
                       "Wisconsin" = track2('Wisconsin'),
                       "Wyoming" = track2('Wyoming'))
        
        data <- as_tibble(data)
        date_input <- input$date
        
        data %>% 
            filter(date <= date_input) %>%
            ggplot(aes(x = date, y = positive)) + 
            geom_line(colour = "#E69F00") +
            geom_point(size=2, colour="#E69F00") +
            theme_ipsum() +
            labs(title = "Cumulative Cases over Time",
                 x = "Date",
                 y = "Confirmed cases")
    })
    
    output$region_plot_cumulative_log <- renderPlot({
        
        data <- switch(input$region, 
                       "US" = usts,
                       "Alabama" = track2("Alabama"),
                       "Alaska" = track2("Alaska"),
                       "Arizona" = track2('Arizona'),
                       "Arkansas" = track2('Arkansas'),
                       "California" = track2('California'),
                       "Colorado" = track2('Colorado'),
                       "Connecticut" = track2('Connecticut'),
                       "Delaware" = track2('Delaware'),
                       "Florida" = track2('Florida'),
                       "Georgia" = track2('Georgia'),
                       "Hawaii" = track2('Hawaii'),
                       "Idaho" = track2('Idaho'),
                       "Illinois" = track2('Illinois'),
                       "Indiana" = track2('Indiana'),
                       "Iowa" = track2('Iowa'),
                       "Kansas" = track2('Kansas'),
                       "Kentucky" = track2('Kentucky'),
                       "Louisiana" = track2('Louisiana'),
                       "Maine" = track2('Maine'),
                       "Maryland" = track2('Maryland'),
                       "Massachusetts" = track2('Massachusetts'),
                       "Michigan" = track2('Michigan'),
                       "Minnesota" = track2('Minnesota'),
                       "Mississippi" = track2('Mississippi'),
                       "Missouri" = track2('Missouri'),
                       "Montana" = track2('Montana'),
                       "Nebraska" = track2('Nebraska'),
                       "Nevada" = track2('Nevada'),
                       "New Hampshire" = track2('New Hampshire'),
                       "New Jersey" = track2('New Jersey'),
                       "New Mexico" = track2('New Mexico'),
                       "New York" = track2('New York'),
                       "North Carolina" = track2('North Carolina'),
                       "North Dakota" = track2('North Dakota'),
                       "Ohio" = track2('Ohio'),
                       "Oklahoma" = track2('Oklahoma'),
                       "Oregon" = track2('Oregon'),
                       "Pennsylvania" = track2('Pennsylvania'),
                       "Rhode Island" = track2('Rhode Island'),
                       "South Carolina" = track2('South Carolina'),
                       "South Dakota" = track2('South Dakota'),
                       "Tennessee" = track2('Tennessee'),
                       "Texas" = track2('Texas'),
                       "Utah" = track2('Utah'),
                       "Vermont" = track2('Vermont'),
                       "Virginia" = track2('Virginia'),
                       "Washington" = track2('Washington'),
                       "West Virginia" = track2('West Virginia'),
                       "Wisconsin" = track2('Wisconsin'),
                       "Wyoming" = track2('Wyoming'))
        
        data <- as_tibble(data)
        date_input <- input$date
        
        data %>% 
            filter(date <= date_input) %>%
            ggplot(aes(x = date, y = positive)) + 
            geom_line(colour = "#E69F00") +
            geom_point(size=2, colour="#E69F00") +
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +
            theme_ipsum() +
            labs(title = "Cumulative Cases over Time (Log Scale)",
                 x = "Date",
                 y = "Confirmed cases")
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)