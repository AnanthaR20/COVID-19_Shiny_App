##### Shiny app for exploring whether we're beating COVID-19 #####

# Working directory
# setwd("C:/Users/XiyuY/Desktop/Xiyu's Folder/stat 479/project/COVID-19 app2")

# Load required packages
require(shiny)
require(dplyr)
require(ggplot2)
require(plotly)
require(hrbrthemes)
require(shinythemes)
require(scales)
require(leaflet) # for creating map
require(htmltools) # for creating map

# Load data
source("pipe3.R")

# Suppress warnings
options(warn=-1)

# Define UI ----
ui <- navbarPage("Are We Beating COVID-19: Is social distancing working?",
                 theme = shinytheme("flatly"),
#_______________________________________________________________________________________________#                       
tabPanel("Visualization",
fluidPage(theme="simplex.min.css",
tags$h3("Data visualization of the growth of COVID-19"),

leafletOutput("mymap"),

fluidRow(
  column(4,
         helpText("Choose a region, date or a plot to see the output result."),
         selectInput("region", 
                     label = "Choose a region",
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
                     timeFormat="%Y-%m-%d"),
         
         selectInput("plot",
                     label = "Choose a plot to display",
                     choices = list("Daily New Cases" = 1, "Cumulative Cases" = 2,
                                    "Cumulative Cases (log)" = 3, "Total vs. New Cases" = 4),
                     selected = 1
           
         )
  ),
  column(8,
         plotOutput("selectedplot", height = "350px")
  )
)
)                            
),
                       
#-------------------------------------------------------------------------------------------------#                       
tabPanel("Our Claims",
        titlePanel("Is social distancing working?"),
        
        tabsetPanel(
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
                     mainPanel(tableOutput("doubling_rate_table"))),
                     # hr(),
                     # 
                     # sidebarLayout(
                     #     sidebarPanel(
                     #         selectInput("region4", 
                     #                     label = "Choose a region to display",
                     #                     choices = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
                     #                                 "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",      
                     #                                 "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",         
                     #                                 "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",      
                     #                                 "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",     
                     #                                 "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                     #                                 "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",         
                     #                                 "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                     #                                 "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                     #                                 "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
                     #                     selected = "US")),
                     #     mainPanel(tableOutput("doubling_rate_table")))),
#_______________________________________________________________________________________________#
            tabPanel("Changes of Slope", 
                     
                     hr(),
                     
                     plotOutput("slope_plot")),

            tabPanel("Prediction of stay home order lag",
         
                    plotOutput(outputId = "predict"),
                    textOutput(outputId = 'summary'),
                    plotOutput(outputId = "predict2"),
                    textOutput(outputId = 'summary2')))),
#_______________________________________________________________________________________________#
            tabPanel("About",
                     tags$br(),tags$h4("Background"),
                     "In December 2019, a virus causing severe respiratory illness has been found in Wuhan, China. In 
                     January 2020, World Health Organization (WHO) identified it as a new coronovirus and officially named it as 
                     COVID-19. The virus was soon transmitted outside of China and spreaded around the world. There are new confirmed cases 
                     death everyday. Until now, effective medicines and treatments combating against COVID-19 are still in the process of 
                     development.",
                     tags$br(),tags$h4("Thesis"), 
                     "For this project, we are interested to see whether we are beating COVID-19 in the US. 
                      Specifically, we focus on how social distancing policy influence COVID-19 in different states. 
                      Our thesis statement is: social distancing policy successfully slow the spread of COVID-19. 
                      This app serves two purpose: First, visualize the growth of COVID-19 in the US; Second, 
                      evaluate our claims.",
                     tags$br(),tags$h4("Data"),
                     "Our data is collected and stored by", tags$a(href="https://covidtracking.com/api", "The COVID Tracking Project. "), "We 
                     have two variables of interests: the total cumulative positive test results and the total cumulative negative test results.",
                     tags$br(),tags$h4("Code"),
                     "Tool and other input data used to generate the shiny app are available on ", tags$a(href="https://github.com/VanR20/COVID-19_Shiny_App", "Github. "),
                     tags$br(), tags$h4("Project Description"),
                     tags$h5("Data Visualization"),
                     "The map in the visualization shows the cumulative positive and negative cases per state. There are four different plots in the visualization 
                     part, including the daily new confirmed cases, cumulative confirmed cases overtime, cumulative confirmed cases over time 
                     (log scale), and total confirmed cases vs. new confirmed cases in the previous week. For the last plot, we're expecting to see 
                     a sheer drop in the curve, which indicates that the growth of COVID-19 slows. An example can be seen in the total vs. new confirmed 
                     cases plot of Hawaii.",
                     tags$br(),tags$h4("Our Claims"),
                     tags$h5("R0 plot"),
                     "We define incidence as the number of new cases in a day. The R0 plot represents a ratio of incidence on
                     day 'i' to day 'i-1'. In this sense it is analogous to an approximated second derivative of the number of positive cases.
                     If the ratio is greater than 1 the number of cases is accelerating. If it is equal to 1 we may be at something of an inflection
                     point. If the ratio is less than 1 the number of cases is decelerating. Each point in this graph represents the average of day-to-day
                     ratios for the immediately preceding week. For example, the data point on 4-20 is the average of the ratios since 4-14. The data point at
                     4-21 is the average of the ratios since 4-15. It is, in a sense, a moving average. We potted the data in this way, because it
                     smoothes out data reporting abnormalities and gives a better sense of when we're in a 'peak' by
                     providing a larger window.", 
                     tags$h5("Doublint-Period Tables"),
                     "In the doubling tables, we have listed the average amount of days until the number of cases
                     doubles. In the states with NA, there has been no stay home order put in effect. As we can see these numbers
                     consistently declined for each week after the order was issued.", 
                     tags$h5("Slope Plot"),
                     "The models fit here are between time and the total number of positive cases. For each line,
                     it is fit to the most recent week-interval defined in the legend. With each succesive week, the slope of the line
                     decreases;indicating a potential slowing of the growth of COVID-19.", 
                     tags$h5("Population Plots"),
                     "We define the 'time until peak', as being the number of days between the stay home order and the first
                     week of r0 values less than 1 (the first week where number of cases are decelerating). Here, we have two plots: 
                     one which makes a model between population and time until peak, and the other with a model between
                     population density and time until peak. We see a significant correlation between population density
                     and the time it takes a stay home order to take effect such that larger densities lead to longer lags until effect.", 
                     )

                       
)


# Define server logic ----
server <- function(input, output) {
  output$mymap <- renderLeaflet({
    
    RegionName <- input$region
    if(RegionName == "US"){
      ViewLon = -98.5795
      ViewLat = 39.8283
      ViewZoom = 4
    }else{
      ViewLon = state_current[state_current$state==RegionName, "Longitude"][1]
      ViewLat = state_current[state_current$state==RegionName, "Latitude"][1]
      ViewZoom = 6
    }
    
    leaflet(state_current) %>% 
      addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>% 
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.35)) %>%
      addProviderTiles(providers$Stamen.TonerLabels) %>% 
      setView(lng = ViewLon, lat = ViewLat, zoom = ViewZoom) %>% 
      addCircles(lng = ~Longitude, lat = ~Latitude, 
                 stroke = FALSE,
                 weight = 1,
                 color = "#0066FF",
                 radius = ~sqrt(negative)*350, 
                 label = ~paste0("Total Negative Cases: ", negative)) %>% 
      addCircles(lng = ~Longitude, lat = ~Latitude, 
                 stroke = T,
                 weight = 1, 
                 color = "#FF0000",
                 radius = ~sqrt(positive)*350, 
                 label = ~paste0("Total Positive Cases: ", positive)) %>% 
      addLegend("topright", 
                colors= c("#0066FF", "#FF0000"), 
                opacity = 0.35,
                labels=c("Negative Cases", "Positive Cases"), 
                title="Cumulative Cases")
    
  })
#_______________________________________________________________________________________________#
    output$selectedplot <- 
      renderPlot({
        if(input$plot == 1){
        data <- NULL
        if(input$region == "US"){ data <- usts }
        else { data <- track2(input$region) }
        
        date_input <- input$date
        
        data.frame(data) %>% 
            filter(date <= date_input) %>%
            mutate(daily_negative = negative - lead(negative, 1)) %>% 
            ggplot(aes(x = date, y = incidence)) + 
            geom_area(fill = "#FF3333", alpha=0.3) +
            geom_line(colour="#FF3333") +
            geom_area(aes(x=date, y=daily_negative),fill = "cornflowerblue", alpha = 0.3) +
            geom_line(aes(x=date, y=daily_negative), colour="cornflowerblue") +
          scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x))) +
            theme_ipsum() +
            labs(title = paste0("Daily Negative vs. Positive Cases in ", input$region),
                 x = "Date",
                 y = "New negative vs. positive cases (daily)")
        
    }
#_______________________________________________________________________________________________#
    else if(input$plot == 2){
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
            labs(title = paste0("Cumulative Cases over Time in ", input$region),
                 x = "Date",
                 y = "Confirmed cases")
  }
#_______________________________________________________________________________________________#   
    else if(input$plot == 3){
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
            labs(title = paste0("Cumulative Cases over Time in ", input$region, " (Log Scale)"),
                 x = "Date",
                 y = "Confirmed cases")
      }
#_______________________________________________________________________________________________#  
    else {
        data <- NULL
        if(input$region == "US"){ data <- usts }
        else { data <- track2(input$region) }
    
        date_input <- input$date
        
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
            labs(title = paste0("Trajectory of Covid-19 confirmed cases in ", input$region),
                 x = "Total confirmed cases",
                 y = "New confirmed cases (previous week)")
    }})
#_______________________________________________________________________________________________#
    output$r0_plot <- renderPlot({
        # data <- NULL
        # if(input$region3 == "US"){ data <- usts }
        # else { data <- track2(input$region3) }
        
        plotState <- NULL
        if(input$region3 != "US"){
          plotState <- input$region3
        }
        
        if(!is.null(plotState)){
        gen_ratio_df(plotState) %>% filter(date <= input$dateR0) %>% ggplot(aes(x = date, y = r0)) + geom_line() + geom_point(aes(color = Decelerating))+
          geom_vline(xintercept = stay_home$date_StayHomeOrder_issued[stay_home$state == plotState],color = "#33aa66")+
          labs(x = "Date", y = "Ratio of day to day Incidence", title = str_c("Acceleration factor of COVID-19 in ",plotState))
        } else {
          gen_ratio_df() %>% filter(date > "2020-02-28") %>% filter(date <= input$dateR0) %>% 
            ggplot(aes(x = date, y = r0)) + 
              geom_line() + geom_point(aes(color = Decelerating))+
           # geom_vline(xintercept = stay_home$date_StayHomeOrder_issued[stay_home$state == plotState],color = "#33aa66")+
              labs(x = "Date", y = "Ratio of day to day Incidence", title = str_c("Acceleration factor of COVID-19 in US"))
        }
        # data %>% filter(date <= input$dateR0)%>% ggplot(aes(x=date,y=R0))+
        #     geom_line(color="#56B4E9", size=1) +
        #     geom_point(size=2, color="#56B4E9")+
        #     geom_vline(xintercept = stay_home$date_StayHomeOrder_issued[stay_home$state == plotState],
        #                color = '#D55E00', size=1.5) +
        #     theme_ipsum_es()+
        #     labs(x = "Date", 
        #          y = "Ratio of incidence today to yesterday",
        #          title = "Examining the acceleration factor of infection")
    })
#_______________________________________________________________________________________________#
    output$doubling_rate_table <- renderTable({
      map_dfr(uskey$state,function(st){
        data <- track2(st)
        
        subtract <- yday(min(data[['date']]))
        
        day1 <- yday(min(data[['date']])) - subtract + 1
        day2 <- yday(stay_home$date_StayHomeOrder_issued[stay_home$state == st]) - subtract
        day3 <- today() - subtract
        
        if(is.na(day2) | is.null(day2)){
          shift <- data.frame(State = st,
                              Before_Stay_At_Home = NA,
                              After_Stay_At_Home = NA,
                              One_Week_After_Order = NA,
                              Two_Weeks_After_Order = NA,
                              One_Month_After_Order = NA)
          return(shift)
        }
        shift <- data.frame(State = st,
                            Before_Stay_At_Home = rdouble2(data,day1,day2),
                            After_Stay_At_Home = rdouble2(data,day2+1,day3),
                            One_Week_After_Order = rdouble2(data,day1,day2+7),
                            Two_Weeks_After_Order = rdouble2(data,day1,day2+14),
                            One_Month_After_Order = rdouble2(data,day1,day2+28))
        
        return(shift)
        
      })
        # data <- NULL
        # if(input$region4 == "US"){ data <- usts}
        # else { data <- track2(input$region4)}
        # 
        # subtract <- yday(min(data[['date']]))
        # 
        # day1 <- yday(min(data[['date']])) - subtract + 1
        # day2 <- yday(stay_home$date_StayHomeOrder_issued[stay_home$state == input$region4]) - subtract
        # day3 <- today() - subtract
        # 
        # shift <- data.frame(Before_Stay_At_Home = rdouble2(data,day1,day2),
        #                     After_Stay_At_Home = rdouble2(data,day2+1,day3))
        # shift
    })    
#_______________________________________________________________________________________________#
    output$slope_plot <- renderPlot({
        #Making Models
        support <- 0:365
        USpop <- rep(2000000,length(support))
        
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
        colnames(USpredictions) <- c("day","2 Million Cases","timeline","last1Week","last2Weeks","last3Weeks",'date')
        USpredictions <- USpredictions %>% pivot_longer(cols = c("2 Million Cases","timeline","last1Week","last2Weeks","last3Weeks"),names_to = "model")
    
        ################## Model prediciton for the US #################
        ####################plots of models#################################
        
        USpredictions %>% ggplot(mapping = aes(x = date,color = model)) +
            geom_line(mapping = aes(y = value)) + labs(x = "Date", y = "# Confirmed Cases",
                                                       title = "Projected trajectory fit over data from specified ranges") +
            xlim(as.Date("2020-03-01"),as.Date("2020-12-31"))
    })
    
    
    output$predict <- renderPlot({
      # Make Time_Until_Control matrix df
      map_dfr(uskey$state,function(st){
        #print(st)
        df <- gen_ratio_df(st)
        #print(df)
        population <- uskey$population[uskey$state == st]
        
        density <- uskey$pop_density[uskey$state == st]
        
        order_day <- stay_home$date_StayHomeOrder_issued[stay_home$state == st]
        
        first <- (df %>% filter(Decelerating == TRUE, 
                                date > order_day))[['date']][1]
        
        #print(first)
        
        effect_time <- yday(first) - yday(order_day)
        
        #print(effect_time)
        
        return(data.frame(state = st,
                          population = population,
                          days_until_control = effect_time,
                          population_density = density))
      }) -> Control_Time
      
      
      # Make model of population to time until runaway growth controlled
      m <- lm(days_until_control~population,data = Control_Time)
      
      Control_Time %>% ggplot(aes(x=population,y =days_until_control))+
        geom_point() + geom_abline(intercept = m$coefficients[[1]],
                                   slope = m$coefficients[[2]],
                                   color = 'red') +
        labs(x = "Population", y = "Days until Peak",
             title = "Stay Home order Lag vs Population")
    })
    
    output$predict2 <- renderPlot({
      # Make Time_Until_Control matrix df
      map_dfr(uskey$state,function(st){
        #print(st)
        df <- gen_ratio_df(st)
        #print(df)
        population <- uskey$population[uskey$state == st]
        
        density <- uskey$pop_density[uskey$state == st]
        
        order_day <- stay_home$date_StayHomeOrder_issued[stay_home$state == st]
        
        first <- (df %>% filter(Decelerating == TRUE, 
                                date > order_day))[['date']][1]
        
        #print(first)
        
        effect_time <- yday(first) - yday(order_day)
        
        #print(effect_time)
        
        return(data.frame(state = st,
                          population = population,
                          days_until_control = effect_time,
                          population_density = density))
      }) -> Control_Time
      
      # Make model of population to time until runaway growth controlled
      m <- lm(days_until_control~population_density,data = Control_Time)
      
      Control_Time %>% ggplot(aes(x=population_density,y = days_until_control))+
        geom_point() + geom_abline(intercept = m$coefficients[[1]],
                                   slope = m$coefficients[[2]],
                                   color = 'red') +
        labs(x = "#People/square mile", y = "Days from Order until Peak",
             title = "Stay Home order Lag vs Population Density")
    })

    output$summary <- renderText({
      # Make Time_Until_Control matrix df
      map_dfr(uskey$state,function(st){
        #print(st)
        df <- gen_ratio_df(st)
        #print(df)
        population <- uskey$population[uskey$state == st]
        
        density <- uskey$pop_density[uskey$state == st]
        
        order_day <- stay_home$date_StayHomeOrder_issued[stay_home$state == st]
        
        first <- (df %>% filter(Decelerating == TRUE, 
                                date > order_day))[['date']][1]
        
        #print(first)
        
        effect_time <- yday(first) - yday(order_day)
        
        #print(effect_time)
        
        return(data.frame(state = st,
                          population = population,
                          days_until_control = effect_time,
                          population_density = density))
      }) -> Control_Time
        
      # Make model of population to time until runaway growth controlled
      m <- lm(days_until_control~population,data = Control_Time)
      
      print(str_c("Equation for the line is -> Days until Peak = \n",
                  m$coefficients[[2]],"  *  Population  +  ",
                  m$coefficients[[1]],".\n   Slope has a significance of 0.111\n
                    Intercept has signficance of 2.72e-09","       This relationship predicts that states with higher population may not necessarily take longer to reach a peak than smaller states. "))
    })
    
    output$summary2 <- renderText({
      # Make Time_Until_Control matrix df
      map_dfr(uskey$state,function(st){
        #print(st)
        df <- gen_ratio_df(st)
        #print(df)
        population <- uskey$population[uskey$state == st]
        
        density <- uskey$pop_density[uskey$state == st]
        
        order_day <- stay_home$date_StayHomeOrder_issued[stay_home$state == st]
        
        first <- (df %>% filter(Decelerating == TRUE, 
                                date > order_day))[['date']][1]
        
        #print(first)
        
        effect_time <- yday(first) - yday(order_day)
        
        #print(effect_time)
        
        return(data.frame(state = st,
                          population = population,
                          days_until_control = effect_time,
                          population_density = density))
      }) -> Control_Time
      
      # Make model of population to time until runaway growth controlled
      m <- lm(days_until_control~population_density,data = Control_Time)
      
      print(str_c("Equation for the line is -> Days until Peak = \n",
                  m$coefficients[[2]],"  *  Population_Density  +  ",
                  m$coefficients[[1]],".\n   Slope has a significance of 0.0408\n
                    Intercept has signficance of 6.62e-10","       This relationship shows that there is a significant relationship between population density and the time it takes for a state to reach a peak in cases. This must be carefully considered when deciding to end stay at home orders."))
    })
    
    
    
}

# Run the app ----
shinyApp(ui = ui, server = server)