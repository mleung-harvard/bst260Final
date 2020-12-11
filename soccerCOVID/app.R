library(RCurl)
library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggthemes)

# read cleaned data
# create league variable, outcome measure, and a pre/post lockdown indicator
df <- read.csv("https://raw.githubusercontent.com/mleung-harvard/bst260Final/main/dataTop5.csv", header=T) %>%
    mutate(Date = as.Date(Date),
           league = as.character(league),
           outcome = ifelse(outcome == "goal_diff", "Goal Difference", "Points per Game (Home)"),
           period = factor(period,
                           levels = c(0,1),
                           labels = c("Pre-lockdown", "Post-lockdown")))


meanDiff <- function(outcome, period, df) {
    m1 <- lm(value ~ period, data = df)
    sprintf(
        "Mean difference: %0.3f [95%% CI: %0.2f, %0.2f]\n p-value: %0.3f",
        summary(m1)$coef[2,1], confint(m1)[2,1], confint(m1)[2,2], summary(m1)$coef[2,4]
    )
}

chisq <-  function(outcome, period, df) {
    m1 <- lm(value ~ period, data = df)
    chisq <- chisq.test(table(df$period, df$value))
    sprintf(
        "Chi-square: %0.3f \n p-value: %0.3f\n\nMean difference: %0.3f [95%% CI: %0.2f, %0.2f]\n p-value: %0.3f",
        chisq$statistic, chisq$p.value, summary(m1)$coef[2,1], confint(m1)[2,1], confint(m1)[2,2], summary(m1)$coef[2,4]
    )
}

shinyApp(
    ui = fluidPage(
        
        # App title
        titlePanel("Soccer Home Advantage in the COVID-19 Era"),
        
        # App theme
        theme = shinytheme("lumen"),
        
        # Create 3 fluid rows
        fluidRow(
            sidebarLayout(
                sidebarPanel(
                    # Add some text and a couple of hyper links before the slider for year
                    p("Due to COVID-19, soccer fans are no longer allowed in stadiums. 
                      We explore whether the lack of fans diminishes home advantage in the top 5 European leagues by comparing
                      home advantage metrics pre- and post-lockdown."),
                    
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
                   h4("Points near click for Goal Difference"),
                   verbatimTextOutput("click_info"))
        )
        
    ),
    
    server = function(input, output) {
        
        output$Plot1 <- renderPlot({
            df1 <- df %>% 
                filter(league %in% input$league,
                       outcome %in% input$outcome) 
            df1 %>%
                ggplot() +
                # define the type of plot
                {
                    if(input$outcome == "Goal Difference")
                    {
                        if(input$league == "spain") 
                            geom_point(aes(Date, value), color="blue")
                        else if(input$league == "england") 
                            geom_point(aes(Date, value), color="red")
                        else if(input$league == "germany") 
                            geom_point(aes(Date, value), color="green4")
                        else if(input$league == "italy") 
                            geom_point(aes(Date, value), color="purple")
                        else
                            geom_point(aes(Date, value), color="hotpink")
                    }
                    else
                    {
                        if(input$league == "spain") 
                            geom_histogram(aes(as.factor(value)), stat = "count", fill="blue", alpha=0.7)
                        else if(input$league == "england") 
                            geom_histogram(aes(as.factor(value)), stat = "count", fill="red", alpha=0.7)
                        else if(input$league == "germany") 
                            geom_histogram(aes(as.factor(value)), stat = "count", fill="green4", alpha=0.7)
                        else if(input$league == "italy") 
                            geom_histogram(aes(as.factor(value)), stat = "count", fill="purple", alpha=0.7)
                        else
                            geom_histogram(aes(as.factor(value)), stat = "count", fill="hotpink", alpha=0.7)
                    }
                        
                } +
                # add vertical line for goal difference
                {
                    if(input$outcome == "Goal Difference")
                        geom_vline(aes(xintercept = as.numeric(as.Date("2020-03-20"))), color = "red")
                } +
                # split into pre and post-lock down for histogram
                {
                    if(input$outcome == "Points per Game (Home)")
                        facet_wrap(~period, nrow=1)
                } +
                # add annotation to vertical line
                {
                    if(input$outcome == "Goal Difference")
                        annotate("text", x = as.Date("2020-03-27"), 
                                 y = 4, 
                                 size = 4, 
                                 angle = 90, 
                                 label = "COVID-19 Lockdown")
                } +
                # add mean lines
                {
                    if(input$outcome == "Goal Difference")
                        geom_line(aes(Date, mean_gd, group = period), color = "black")
                } +
                # add labels
                {
                    if(input$outcome == "Goal Difference")
                        labs(x = "Date", y = sprintf(input$outcome))
                    else
                        labs(x = sprintf(input$outcome), y = "Count")
                } +
                theme_bw() +
                theme(legend.position = "none")
        })
        
        output$pvalue <- renderText({
            df3 <- df %>% 
                filter(league %in% input$league,
                       outcome %in% input$outcome) 
            {
                if(input$outcome == "Goal Difference")
                    meanDiff(value, period, df3)
                else
                    chisq(value, period, df3)
                }
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