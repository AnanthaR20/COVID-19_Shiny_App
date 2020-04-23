##### Shiny app for exploring whether we're beating COVID-19 #####

# Load required packages
require(shiny)
require(dplyr)
require(ggplot2)
require(plotly)
require(ggthemes) # not used for now
require(hrbrthemes)
require(shinythemes)
require(scales)

# Load data
source("pipe3.R")

# Define UI ----
ui <- navbarPage("Are We Beating COVID-19: Is social distancing working?",
                 theme = shinytheme("flatly"),
#_______________________________________________________________________________________________#                       
tabPanel("Visualization",
                                
hr(),
                                
sidebarLayout(sidebarPanel(helpText("Choose a region and date to see the output plot."),
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
                                        max = today(),
                                        value= today()-1,
                                        timeFormat="%Y-%m-%d")),
                                    
                mainPanel(titlePanel("Growth of COVID-19"),
                          tabsetPanel(tabPanel("New", plotOutput("region_plot")),
                                      tabPanel("Cumulative", plotOutput("region_plot_cumulative")),
                                      tabPanel("Cumulative (log10)", plotOutput("region_plot_cumulative_log")))))),
                       
#-------------------------------------------------------------------------------------------------#                       
tabPanel("Our Claims",
        titlePanel("Is social distancing working?"),
        
        tabsetPanel(
            
            tabPanel("Total vs. New (previous week)", 
                                             
            hr(),
                                             
            sidebarLayout(
                         sidebarPanel(
                             selectInput("region2", 
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
                             
                             sliderInput("date2",
                                         label = "Dates",
                                         min = as.Date("2020-03-01","%Y-%m-%d"),
                                         max = today(),
                                         value= today()-1,
                                         timeFormat="%Y-%m-%d")),
                         
                         mainPanel(plotOutput("total_vs_new_plot")))),
#_______________________________________________________________________________________________#
            tabPanel("R_0 Graph", 
                     
                     hr(),
                     
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("region3", 
                                         label = "Choose a region to display",
                                         choices = c("US","Alabama", "Alaska", "Arizona", "Arkansas", "California",
                                                     "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",      
                                                     "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",         
                                                     "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",      
                                                     "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",     
                                                     "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                                                     "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",         
                                                     "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                                     "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                                                     "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
                                         selected = "Wisconsin"),
                             
                             sliderInput(inputId = "dateR0",
                                         label = "Selected a date range:",
                                         min = as.Date("2020-03-01"),
                                         max = today(),
                                         value= today()-1,
                                         timeFormat="%Y-%m-%d")),
                     
                     mainPanel(plotOutput("r0_plot")))),
#_______________________________________________________________________________________________#
            tabPanel("Shift in Doubling Rate", 
                     
                     hr(),
                     
                     sidebarLayout(
                         sidebarPanel(
                             selectInput("region4", 
                                         label = "Choose a region to display",
                                         choices = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                                                     "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",      
                                                     "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",         
                                                     "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",      
                                                     "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",     
                                                     "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                                                     "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",         
                                                     "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                                                     "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                                                     "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
                                         selected = "US")),
                         mainPanel(tableOutput("doubling_rate_table")))),
#_______________________________________________________________________________________________#
            tabPanel("Changes of Slope", 
                     
                     hr(),
                     
                     plotOutput("slope_plot")))),
                       
                       
#_______________________________________________________________________________________________#
            tabPanel("About",)

                       
)


# Define server logic ----
server <- function(input, output) {
    
    output$region_plot <- renderPlot({
        data <- NULL
        if(input$region == "US"){ data <- usts }
        else { data <- track2(input$region) }
        
        date_input <- input$date
        
        data.frame(data) %>% 
            filter(date <= date_input) %>%
            ggplot(aes(x = date, y = incidence)) + 
            geom_area(fill = "#69b3a2", alpha=0.5) +
            geom_line(colour="#69b3a2") +
            theme_ipsum() +
            labs(x = "Date",
                 y = "New cases (daily)")
        
    })
#_______________________________________________________________________________________________#
    output$region_plot_cumulative <- renderPlot({
        data <- NULL
        if(input$region == "US"){ data <- usts }
        else { data <- track2(input$region) }
        
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
#_______________________________________________________________________________________________#   
    output$region_plot_cumulative_log <- renderPlot({
        data <- NULL
        if(input$region == "US"){ data <- usts }
        else { data <- track2(input$region) }
        
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
#_______________________________________________________________________________________________#  
    output$total_vs_new_plot <- renderPlot({
        data <- NULL
        if(input$region2 == "US"){ data <- usts }
        else { data <- track2(input$region2) }
    
        date_input <- input$date2
        
        data %>% 
            filter(date <= date_input) %>%
            select(date, positive) %>% 
            mutate(new_prewk = (positive - lead(positive, n=6))) %>% 
            ggplot(aes(x = positive, y = new_prewk)) + 
            geom_line(colour = "#E69F00") +
            geom_point(size=2, colour="#E69F00") +
            scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x))) +
            theme_ipsum_es() +
            labs(title = "Trajectory of Covid-19 confirmed cases",
                 x = "Total confirmed cases",
                 y = "New confirmed cases (previous week)")
    })
#_______________________________________________________________________________________________#
    output$r0_plot <- renderPlot({
        data <- NULL
        if(input$region3 == "US"){ data <- usts }
        else { data <- track2(input$region3) }
        
        plotState <- input$region3
        
        data %>% filter(date <= input$dateR0)%>% ggplot(aes(x=date,y=R0))+
            geom_line(color="#56B4E9", size=1) +
            geom_point(size=2, color="#56B4E9")+
            geom_vline(xintercept = stay_home$date_StayHomeOrder_issued[stay_home$state == plotState],
                       color = '#D55E00', size=1.5) +
            theme_ipsum_es()+
            labs(x = "Date", 
                 y = "Ratio of incidence today to yesterday",
                 title = "Examining the acceleration factor of infection")
    })
#_______________________________________________________________________________________________#
    output$doubling_rate_table <- renderTable({
        data <- NULL
        if(input$region4 == "US"){ data <- usts}
        else { data <- track2(input$region4)}
        
        subtract <- yday(min(data[['date']]))
        
        day1 <- yday(min(data[['date']])) - subtract + 1
        day2 <- yday(stay_home$date_StayHomeOrder_issued[stay_home$state == input$region4]) - subtract
        day3 <- today() - subtract
        
        shift <- data.frame(Before_Stay_At_Home = rdouble2(data,day1,day2),
                            After_Stay_At_Home = rdouble2(data,day2+1,day3))
        shift
    })    
#_______________________________________________________________________________________________#
    output$slope_plot <- renderPlot({
        #Making Models
        support <- 0:365
        USpop <- rep(1000000,length(support))
        
        m <- lm(positive ~ day , data = usts)
        m1 <- lm(positive ~ day, data = (usts %>% filter(date > today()-7)))
        m2 <- lm(positive ~ day, data = (usts %>% filter(date > today()-14)))
        m3 <- lm(positive ~ day, data = (usts %>% filter(date > today()-21)))
        
        f <- function(x){return(m$coefficients[[1]] + m$coefficients[[2]]*x)}
        f1 <- function(x){return(m1$coefficients[[1]] + m1$coefficients[[2]]*x)}
        f2 <- function(x){return(m2$coefficients[[1]] + m2$coefficients[[2]]*x)}
        f3 <- function(x){return(m3$coefficients[[1]] + m3$coefficients[[2]]*x)}
        
        USpredictions <- data.frame(matrix(nrow = length(support),ncol = 6))
        USpredictions[[1]] <- support
        USpredictions[[2]] <- USpop
        USpredictions[[3]] <- f(support)
        USpredictions[[4]] <- f1(support)
        USpredictions[[5]] <- f2(support)
        USpredictions[[6]] <- f3(support)
        USpredictions[[7]] <- as.Date("2020-02-28") + (support)
        colnames(USpredictions) <- c("day","1 Million Cases","timeline","last1Week","last2Weeks","last3Weeks",'date')
        USpredictions <- USpredictions %>% pivot_longer(cols = c("1 Million Cases","timeline","last1Week","last2Weeks","last3Weeks"),names_to = "model")
    
        ################## Model prediciton for the US #################
        ####################plots of models#################################
        
        USpredictions %>% ggplot(mapping = aes(x = date,color = model)) +
            geom_line(mapping = aes(y = value)) + labs(x = "Date", y = "# Confirmed Cases",
                                                       title = "Projected trajectory fit over data from specified ranges") +
            xlim(as.Date("2020-03-01"),as.Date("2020-12-31"))
    })
    
        
}

# Run the app ----
shinyApp(ui = ui, server = server)