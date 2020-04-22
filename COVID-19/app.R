##### Shiny app for exploring whether we're beating COVID-19 #####

# Load required packages
require(shiny)
require(dplyr)
require(ggplot2)
require(ggthemes)
require(shinythemes)
require(scales)

# Load data and helper files
source("dataTables.R")
#source("pipe3.R")

# Define UI ----
ui <- ui <- navbarPage("Are We Beating COVID-19: Visualization and Prediction",
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
                                                    min = as.Date("2020-01-22","%Y-%m-%d"),
                                                    max = as.Date("2020-04-21","%Y-%m-%d"),
                                                    value=as.Date("2020-04-21","%Y-%m-%d"),
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
                       
                       
                       
                       navbarMenu("Prediction",
                                  tabPanel("Are we flattening the curve?",
                                           titlePanel("Are we flattening the curve?"),
                                  ),
                                  tabPanel("Is social distancing working?",
                                           titlePanel("Is social distancing working?"),
                                  ),
                                  tabPanel("Are we ready for the next outbreak?",
                                           titlePanel("Are we ready for the next outbreak?"),
                                  )
                                  
                       ),
                       
                       
                       
                       tabPanel("About",
                                
                       )
                       
)


# Define server logic ----
server <- function(input, output) {
    output$region_plot_cumulative <- renderPlot({
        
        data <- switch(input$region, 
                       "US" = c$US,
                       "California" = s$California,
                       "New York" = s$`New York`,
                       "Washington" = s$Washington,
                       "Wisconsin" = s$Wisconsin)
        
        data <- as_tibble(data)
        date_input <- input$date
        
        data %>% 
            filter(date <= date_input) %>%
            ggplot(aes(x = date, y = confirms)) + 
            geom_line(colour = "#E69F00") +
            geom_point(size=2, colour="#E69F00") +
            theme_grey() +
            labs(title = "Cumulative Cases over Time",
                 x = "Date",
                 y = "Confirmed cases")
    })
    
    output$region_plot_cumulative_log <- renderPlot({
        
        data <- switch(input$region, 
                       "US" = c$US,
                       "Alabama" = s$Alabama,
                       "Alaska" = s$Alaska,
                       "Arizona" = s$Arizona,
                       "Arkansas" = s$Arkansas,
                       "California" = s$California,
                       "Colorado" = s$Colorado,
                       "Connecticut" = s$Connecticut,
                       "Delaware" = s$Delaware,
                       "Florida" = s$Florida,
                       "Georgia" = s$Georgia,
                       "Hawaii" = s$Hawaii,
                       "Idaho" = s$Idaho,
                       "Illinois" = s$Illinois,
                       "Indiana" = s$Indiana,
                       "Iowa" = s$Iowa,
                       "Kansas" = s$Kansas,
                       "Kentucky" = s$Kentucky,
                       "Louisiana" = s$Louisiana,
                       "Maine" = s$Maine,
                       "Maryland" = s$Maryland,
                       "Massachusetts" = s$Massachusetts,
                       "Michigan" = s$Michigan,
                       "Minnesota" = s$Minnesota,
                       "Mississippi" = s$Mississippi,
                       "Missouri" = s$Missouri,
                       "Montana" = s$Montana,
                       "Nebraska" = s$Nebraska,
                       "Nevada" = s$Nevada,
                       "New Hampshire" = s$`New Hampshire`,
                       "New Jersey" = s$`New Jersey`,
                       "New Mexico" = s$`New Mexico`,
                       "New York" = s$`New York`,
                       "North Carolina" = s$`North Carolina`,
                       "North Dakota" = s$`North Dakota`,
                       "Ohio" = s$Ohio,
                       "Oklahoma" = s$Oklahoma,
                       "Oregon" = s$Oregon,
                       "Pennsylvania" = s$Pennsylvania,
                       "Rhode Island" = s$`Rhode Island`,
                       "South Carolina" = s$`South Carolina`,
                       "South Dakota" = s$`South Dakota`,
                       "Tennessee" = s$Tennessee,
                       "Texas" = s$Texas,
                       "Utah" = s$Utah,
                       "Vermont" = s$Vermont,
                       "Virginia" = s$Virginia,
                       "Washington" = s$Washington,
                       "West Virginia" = s$`West Virginia`,
                       "Wisconsin" = s$Wisconsin,
                       "Wyoming" = s$Wyoming)
        
        data <- as_tibble(data)
        date_input <- input$date
        
        data %>% 
            filter(date <= date_input) %>%
            ggplot(aes(x = date, y = confirms)) + 
            geom_line(colour = "#E69F00") +
            geom_point(size=2, colour="#E69F00") +
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +
            theme_grey() +
            labs(title = "Cumulative Cases over Time",
                 x = "Date",
                 y = "Confirmed cases")
    })
    
}

# Run the app ----
shinyApp(ui = ui, server = server)