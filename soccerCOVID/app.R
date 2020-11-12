library(RCurl)
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

df <- read.csv("https://raw.githubusercontent.com/mleung-harvard/bst260Final/main/dataTop5.csv", header=T) %>%
    mutate(Date = as.Date(Date),
           league = as.character(league),
           outcome = ifelse(outcome == "goal_diff", "Goal Difference", "Points per Game (Home)")) 

m1 <- lm(value ~ period, data = df)

confint(m1)[2,2]


meanDiff <- function(outcome, period, df) {
    m1 <- lm(value ~ period, data = df)
    sprintf(
        "Mean difference: %0.3f [95%% CI: %0.2f, %0.2f]\n pvalue: %0.3f",
        summary(m1)$coef[2,1], confint(m1)[2,1], confint(m1)[2,2], summary(m1)$coef[2,4]
    )
}

shinyApp(
    ui = fluidPage(
        
        # App title
        titlePanel("Soccer Home Advantage in the COVID-19 Era"),
        theme = shinytheme("lumen"),
        
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
                    plotOutput("Plot1",
                               click = "plot_clicked")
                    
                )
            )),
        fluidRow(
            column(width = 12,
                   h4("Summary Statistics"),
                   verbatimTextOutput("pvalue")
            
            )),
        fluidRow(
            column(width = 12,
                   h4("Points near click"),
                   verbatimTextOutput("click_info"))
        )
        
    ),
    
    server = function(input, output) {
        
        output$Plot1 <- renderPlot({
            df1 <- df %>% 
                filter(league %in% input$league,
                       outcome %in% input$outcome) 
            df1 %>%
                ggplot(aes(Date, value)) +
                geom_point(color = "Blue") +
                geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-20"))), color = "red") +
                {
                    if(input$outcome == "Goal Difference") 
                        annotate("text", x = as.Date("2020-03-27"), 
                                 y = 4, 
                                 size = 4, 
                                 angle = 90, 
                                 label = "COVID-19 Lockdown")
                    else
                        annotate("text", x = as.Date("2020-03-27"), 
                                 y = 2, 
                                 size = 4, 
                                 angle = 90, 
                                 label = "COVID-19 Lockdown")
                } +
                {
                    if(input$outcome == "Goal Difference") 
                        geom_line(aes(Date, mean_gd, group = period), color = "black") 
                    else 
                        geom_line(aes(Date, mean_ppg, group = period), color = "black")
                } +

                labs(x = "Date", y = sprintf(input$outcome), color = "Period") +
                theme_bw() +
                theme(legend.position = "none")
        })
        
        output$pvalue <- renderText({
            df3 <- df %>% 
                filter(league %in% input$league,
                       outcome %in% input$outcome) 
            meanDiff(value, period, df3)
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