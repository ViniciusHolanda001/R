
library(readr)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)
library(plotly)
library(ggplot2)
library(tidyverse)
library(skimr)


df <- read.csv("data\telecom_churn.csv")


plot(df)
plot(df$Total.day.calls, df$Total.day.charge, color = as.factor(df$State))



df %<>% 
  rename_with(~ tolower(gsub('.', '_', .x, fixed = TRUE)))


sum(is.na(df))


df %>% 
  count(churn)


df %>% 
  group_by(state, churn) %>% 
  summarise(
    mean_calls_day = mean(total_day_calls),
    mean_charge_day = mean(total_day_charge)
  )

skimr::skim(df)

df



plot_calls <- df %>%   
  ggplot(aes(total_day_calls, total_day_charge, colour = as.factor(churn))) +
  geom_point() +
  # geom_smooth(method = 'lm') +
  labs(
    title = 'Total of calls and Charge per day',
    subtitle = 'By client',
    colour = 'Churn',
    y = 'Charge',
    x = 'Calls'
  )

plot_calls


plot_hist <- df %>% 
  ggplot(aes(state, fill = churn)) +
  geom_bar() +
  # geom_smooth(method = 'lm') +
  labs(
    title = 'Total of churn',
    subtitle = 'By state',
    fill = 'Churn',
    y = 'Clients',
    x = 'State'
    
  )

plot_hist


plot_fun <- df %>% 
  ggplot(aes(y=fct_reorder(state, total_day_minutes), color=churn)) +
  geom_bar()

plot_fun



plot_lin <- df %>% 
  ggplot(aes(total_day_minutes, state, colour = churn)) +
  geom_line()

plot_lin



plot_histogram <- df %>% 
  ggplot(aes(x = total_day_calls, y = total_night_calls, color = state)) +
  geom_point()
facet_wrap(~ churn)

plot_histogram



plot_hist_stat <- df %>% 
  ggplot(aes(total_day_charge)) +
  geom_histogram(aes(y = stat(count)/sum(count)), bins = 10) +
  facet_wrap(~ international_plan)

plot_hist_stat

