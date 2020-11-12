library(RCurl)
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/mleung-harvard/bst260Final/main/dataTop5.csv", header=T) %>%
    mutate(Date = as.Date(Date),
           league = as.character(league),
           outcome = ifelse(outcome == "goal_diff", "Goal Difference", "Points per Game (Home)"),
           period = ifelse(period == 0, "Pre-Lockdown", "Post-Lockdown")) 

shinyApp(
    ui = fluidPage(
        
        # App title
        titlePanel("Soccer Home Advantage in the COVID-19 Era"),
        theme = shinytheme("simplex"),
        
        # Create an app with 2 tabs
             fluidRow(
                 sidebarLayout(
                     sidebarPanel(
                         # Add some text and a couple of hyper links before the slider for year
                         p("Test"),
                         
                         # Add some space between the text above and animated
                         # slider bar below
                         br(),
                         
                         # Sidebar with a slider input for number of bins 
                         radioButtons("league",
                                      label = "Select League",
                                      choices = list("Spain" = "spain",
                                                       "England" = "england",
                                                       "Germany" = "germany",
                                                       "Italy" = "italy",
                                                       "France" = "france"),
                                      selected = "spain"),
                         
                         radioButtons("outcome",
                                      "Home Advantage Metric",
                                      choices = list("Goal Difference" = "Goal Difference", 
                                                     "Points per Game (Home)" = "Points per Game (Home)"),
                                      selected = "Goal Difference")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         plotOutput("distPlot",
                                    click = "plot_clicked")
                         
                     )
                 )),
             fluidRow(
                 column(width = 12,
                        h4("Points near click"),
                        verbatimTextOutput("click_info")
                 ))

        ),
    
    server = function(input, output) {
        
        output$distPlot <- renderPlot({
            df1 <- df %>% 
                filter(league %in% input$league,
                       outcome %in% input$outcome) 
            df1 %>%
                ggplot(aes(Date, value)) +
                geom_point(color = "Blue") +
                {
                    if(input$outcome == "Goal Difference") geom_line(aes(Date, mean_gd, group = period), color = "red") 
                    else geom_line(aes(Date, mean_ppg, group = period), color = "red")
                } +
                labs(x = "Date", y = sprintf(input$outcome), color = "Period") +
                theme_bw() +
                theme(legend.position = "none")
        })
        
        
        output$click_info <- renderPrint({
            df2 <- df %>% 
                filter(league %in% input$league,
                       outcome %in% input$outcome) %>%
                select(c(Date, result, local.team, away.team, league, outcome, value))
            nearPoints(df2, input$plot_clicked)
        })
        
        
    }
)