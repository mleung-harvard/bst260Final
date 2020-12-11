rm(list=ls())

library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(data.table)
library(lubridate)
library(shiny)

#############################################################
##### 1. Function to scrape and process soccerstats.com #####
#############################################################

scrape <- function(url1, url2, country, lockdown)
{
  ## url1: url for 2019 season
  ## url2: url for 2020 season
  ## country: country of league
  ## lockdown: date of lockdown
  
  df1 <- read_html(url1) %>% 
    html_nodes("table") %>%
    .[[14]] %>%
    html_table(fill=T) %>%
    select(X1, X3, X2, X4) %>%
    setNames(c("Date", "result", "local.team", "away.team")) %>%
    mutate(season = 2019)
  df2 <- read_html(url2) %>% 
    html_nodes("table") %>%
    .[[14]] %>%
    html_table(fill=T) %>%
    select(X1, X3, X2, X4) %>%
    setNames(c("Date", "result", "local.team", "away.team")) %>%
    mutate(season = 2020)
  df <- df1 %>%
    bind_rows(df2) %>%
    mutate(league = quo_name(enquo(country)),
           result = ifelse(str_detect(result, "pp.|:") == T, NA,
                           ifelse(result == "", NA, result))) %>%
    na.omit() %>%
    mutate(total.goals = str_replace_all(result, " - ", ""),
           local.goals = as.numeric(substr(total.goals, 1, 1)),
           away.goals = as.numeric(substr(total.goals, 2, 2)),
           goal_diff = local.goals - away.goals,
           ppg = case_when(
             local.goals > away.goals ~ 3,
             local.goals == away.goals ~ 1,
             local.goals < away.goals ~ 0), # assigned 3 points to the locals if a win, 1 if a tie, 0 if a defeat
           date2 = parse_date_time(Date, orders = c("dm")),
           month = strftime(date2, "%m"),
           day = strftime(date2, "%d"),
           year = case_when (
             season == 2019 & month %in% c("08", "09", "10", "11", "12") ~ "2019",
             season == 2020 ~ "2020",
             TRUE ~ "2020"), # create season variable
           Date = ymd(paste(year, month, day, sep= '-')),
           period = ifelse(Date <= as.Date(lockdown), 0, 1)) %>%
    select(-c(total.goals, date2, month, day, year))
  
  avg <- df %>%
    group_by(league, period) %>%
    summarise(mean_gd = mean(goal_diff, na.rm = T), mean_ppg = mean(ppg, na.rm = T), .groups = 'drop') %>%
    ungroup()
  
  df <- df %>%
    left_join(avg, by = c("league", "period"))
  
  # return df to global environment
  assign(deparse(substitute(country)), df, envir = .GlobalEnv)
  
}

#################################################
##### 2. Scrape and process soccerstats.com #####
#################################################

scrape("https://www.soccerstats.com/results.asp?league=spain_2020&pmtype=bydate", 
       "https://www.soccerstats.com/results.asp?league=spain&pmtype=bydate", spain, "2020-03-09")
scrape("https://www.soccerstats.com/results.asp?league=england_2020&pmtype=bydate", 
       "https://www.soccerstats.com/results.asp?league=england&pmtype=bydate", england, "2020-03-09")
scrape("https://www.soccerstats.com/results.asp?league=germany_2020&pmtype=bydate", 
       "https://www.soccerstats.com/results.asp?league=germany&pmtype=bydate", germany, "2020-03-11")
scrape("https://www.soccerstats.com/results.asp?league=italy_2020&pmtype=bydate", 
       "https://www.soccerstats.com/results.asp?league=italy&pmtype=bydate", italy, "2020-03-09")
scrape("https://www.soccerstats.com/results.asp?league=france_2020&pmtype=bydate", 
       "https://www.soccerstats.com/results.asp?league=france&pmtype=bydate", france, "2020-03-08")

# convert from wide to long
all <- rbind(spain, england, germany, italy, france) %>%
  gather(outcome, value, c("goal_diff", "ppg"))

write.csv(all, "dataTop5.csv")

