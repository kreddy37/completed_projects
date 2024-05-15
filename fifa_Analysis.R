setwd("C:/Users/kohlr/Downloads")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(dslabs)
library(ggthemes)
library(ggrepel)
filename <- 'players_22.csv'
filename2 <- 'players_21.csv'
filename2 %in% list.files()

dat <- read_csv(filename)
dat2 <- read_csv(filename2)
dat |> select(short_name, league_name) |> head()
sum_dat <- dat |> select( short_name, player_positions, overall, age, height_cm, weight_kg, club_name,
                         league_name, nationality_name, weak_foot, skill_moves, pace, shooting, dribbling, defending,
                         physic) |> filter(nationality_name == 'Brazil')

sum_dat2 <- dat2 |> select(short_name, overall, league_name, nationality_name) |>
  rename('overall_21' = overall) |> filter(nationality_name == 'Brazil')

all_dat <- inner_join(sum_dat, sum_dat2, by = "short_name") |> mutate(rating_change = overall - overall_21) |> 
  select(short_name, overall, age, club_name, league_name.x, rating_change)

league_stats <- all_dat |> group_by(league_name.x) |> filter(league_name.x %in% c('German 1. Bundesliga', 'English Premier League', 'French Ligue 1', 'Spain Primera Division', 'Italian Serie A')) |>
  summarize(average_overall = mean(overall),
            stdev_overall = sd(overall),
            average_age = mean(age),
            stdev_age = sd(age),
            avg_rc = mean(rating_change),
            stdev_rc = sd(rating_change))
league_stats

club_stats <- all_dat |> group_by(club_name) |> summarize(average_overall = mean(overall),
                                                          stdev_overall = sd(overall),
                                                          average_age = mean(age),
                                                          stdev_age = sd(age))
club_stats
#If the standard deviation is NA it's because there is only one Brazilian player in that league/on that team

summary_stats <- all_dat |> summarize(average_ovr = mean(overall), stdev_ovr = sd(overall),
                                      average_age = mean(age), stdev_age = sd(age),
                                      average_rc = mean(rating_change), stdev_rc = sd(rating_change))
summary_stats


all_dat |> filter(league_name.x %in% c('German 1. Bundesliga', 'English Premier League', 'French Ligue 1', 'Spain Primera Division', 'Italian Serie A')) |>
  ggplot(aes(age, rating_change)) +
  geom_point(aes(col = league_name.x), size = 1) +
  xlab('Age') +
  ylab('Rating change') +
  ggtitle('Age vs. Rating change for Brazilian FIFA 22 Players') +
  scale_color_discrete(name = 'Club')

all_dat |> filter(league_name.x %in% c('German 1. Bundesliga', 'English Premier League', 'French Ligue 1', 'Spain Primera Division', 'Italian Serie A')) |>
  ggplot(aes(rating_change)) +
  geom_histogram(binwidth = 1, fill = 'blue', col = 'black') +
  xlab('Overall') +
  ggtitle('Ratings changes for Brazilian players')

all_dat |> filter(league_name.x %in% c('German 1. Bundesliga', 'English Premier League', 'French Ligue 1', 'Spain Primera Division', 'Italian Serie A')) |> ggplot(aes(league_name.x, rating_change)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 23, size = 4) +
  xlab('League name') +
  ylab('Rating change') +
  ggtitle('Rating changes across different leagues for Brazilian players')

