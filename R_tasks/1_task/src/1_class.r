
library(readr)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)
library(magrittr)


## Read dataset


df <- read_csv('data/Summary_of_Weather.csv')


## Summary data


summary(df)
spec(df)
head(df, 10)

# Col
length(df)

sapply(df, function(x) sum(is.na(x)))

head(is.na(df))


df1 <- data.frame(df)

# Just another way to separate and input data to new columns.
# Totally unnecessary!!
date <- str_split_fixed(df1$Date, "-", 3)
df1$Date1 <- date[,1]
df1$Month1 <- date[,2]
df1$Day1 <- date[,3]


group_precip <- df %<>%
  group_by(Date, YR)

df %>% 
  filter(Precip > 5) %>% 
  filter(YR > 44)

# NA values in Precip = mean(Precip)
df$Precip[is.na(df$Precip)] <- mean(df$Precip, na.rm = TRUE)

sum(is.na(df))
sapply(df, function(x) sum(is.na(x)))



# The "T" data in Precip column is about the rain precipitation. When "T" the
# is greater than 1.000
df %<>% 
  mutate(
    Precip = as.double(Precip),
    Precip = coalesce(Precip, mean(df$Precip, na.rm = TRUE))
  )


df %>% 
  summarise(
    MO = MO,
    Precip = mean(Precip)
  ) %>% 
  filter(Precip > 10)



library(ggplot2)

# That's the count of data for each year
ggplot(data = df) +
  geom_bar(mapping = aes(x = YR))

ggplot(data = df, mapping = aes(x = MaxTemp)) +
  geom_histogram(binwidth = 0.1)

df %>% 
  ggplot(aes(Precip, MaxTemp, color = as.factor(YR))) +
  geom_point() +
  geom_smooth()

df %>% 
  ggplot(aes(Date, MaxTemp, MinTemp, color = as.factor(YR))) +
  geom_col()

